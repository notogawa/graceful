module System.Posix.GracefulSpec ( spec ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Network
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Posix.Signals
import System.Posix.Process
import Test.Hspec

spec :: Spec
spec = do
  describe "1st version" $ do
    it "build" build1stVersion
    it "simple access and quit (SIGQUIT)" $ run "/tmp/echo-server" $ simpleAccessAnd sigQUIT
    it "simple access and stop (SIGINT)"  $ run "/tmp/echo-server" $ simpleAccessAnd sigINT
    it "simple access and stop (SIGTERM)" $ run "/tmp/echo-server" $ simpleAccessAnd sigTERM
    it "quit (SIGQUIT) while access" $ run "/tmp/echo-server" quitWhileAccess
    it "stop (SIGINT)  while access" $ run "/tmp/echo-server" $ stopWhileAccess sigINT
    it "stop (SIGTERM) while access" $ run "/tmp/echo-server" $ stopWhileAccess sigTERM

removeFileIfExist :: FilePath -> IO ()
removeFileIfExist path = do
  exist <- doesFileExist path
  when exist $ removeFile path

waitStandby :: FilePath -> IO ()
waitStandby path = tryIO (readFile path) >>=
                   either (\_ -> waitStandby path) (\_ -> threadDelay 1000)

removeFiles :: FilePath -> IO ()
removeFiles file = do
  removeFileIfExist $ file ++ ".sock"
  removeFileIfExist $ file ++ ".pid"

run :: String -> IO () -> IO ()
run file action = do
  removeFiles file
  bracket (rawSystem file []) (\_ -> kill' sigINT) $ \code -> do
    code `shouldBe` ExitSuccess
    waitStandby $ file ++ ".pid"
    action

kill :: Signal -> IO ()
kill signal = readFile "/tmp/echo-server.pid" >>=
              signalProcess signal . read

kill' :: Signal -> IO ()
kill' signal = do
  pid <- readFile "/tmp/echo-server.pid"
  void $ tryIO $ signalProcess signal $ read pid
  threadDelay 10000
  void $ tryIO $ getProcessStatus True False $ read pid

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

shouldEcho :: Handle -> String -> IO ()
shouldEcho h str = do
  hPutStrLn h str
  hFlush h
  hGetLine h `shouldReturn` str

simpleAccess :: IO ()
simpleAccess = access (`shouldEcho` "hello")

access :: (Handle -> IO ()) -> IO ()
access action = bracket (connectTo "localhost" (PortNumber 8080)) hClose action

build :: IO ExitCode
build = rawSystem "ghc" [ "--make", "test/echo.hs", "-o", "/tmp/echo-server" ]

build1stVersion :: IO ()
build1stVersion = build `shouldReturn` ExitSuccess

simpleAccessAnd :: Signal -> IO ()
simpleAccessAnd s = simpleAccess >> kill s

quitWhileAccess :: IO ()
quitWhileAccess = access $ \h -> do
                    kill sigQUIT
                    shouldEcho h "quitWhileAccess"

stopWhileAccess :: Signal -> IO ()
stopWhileAccess s = do
  res <- tryIO $ access $ \h -> do
                    kill s
                    shouldEcho h $ replicate 10240 ' '
  either (\_ -> True) (\_ -> False) res `shouldBe` True
