module System.Posix.GracefulSpec ( spec ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Network
import Network.Socket
import System.Cmd
import System.Directory
import System.Exit
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
removeFiles file = mapM_ (removeFileIfExist . (file ++)) [ ".sock", ".pid" ]

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
kill' signal = readFile "/tmp/echo-server.pid" >>=
               void . tryIO . signalProcess signal . read

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

echo :: Socket -> String -> IO String
echo sock str = send sock str >> recv sock (2 * length str)

simpleAccess :: IO ()
simpleAccess = access $ \sock -> echo sock "simpleAccess" `shouldReturn` "simpleAccess"

access :: (Socket -> IO ()) -> IO ()
access action = bracket (socket AF_INET Stream 0) close $ \sock -> do
                  addr <- inet_addr "127.0.0.1"
                  connect sock (SockAddrInet 8080 addr)
                  action sock

build :: IO ExitCode
build = rawSystem "ghc" [ "--make", "test/echo.hs", "-o", "/tmp/echo-server" ]

ps :: IO ExitCode
ps = rawSystem "ps" [ "-C", "echo-server" ]

build1stVersion :: IO ()
build1stVersion = build `shouldReturn` ExitSuccess

simpleAccessAnd :: Signal -> IO ()
simpleAccessAnd s = simpleAccess >> kill s

quitWhileAccess :: IO ()
quitWhileAccess = access $ \sock -> do
                    kill sigQUIT
                    echo sock "quitWhileAccess" `shouldReturn` "quitWhileAccess"

stopWhileAccess :: Signal -> IO ()
stopWhileAccess s = do
  res <- tryIO $ access $ \sock -> do
                    kill s
                    forever $ echo sock "stopWhileAccess"
  either (\_ -> True) (\_ -> False) res `shouldBe` True
