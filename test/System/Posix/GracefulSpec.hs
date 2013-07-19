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
import Test.Hspec

spec :: Spec
spec = do
  describe "graceful" $ do
    it "build echo as echo" $ buildAsEchoServer "test/echo.hs"
    it "simple access and quit (SIGQUIT)" $ run "/tmp/echo-server" $ simpleAccessAnd sigQUIT
    it "simple access and stop (SIGINT)"  $ run "/tmp/echo-server" $ simpleAccessAnd sigINT
    it "simple access and stop (SIGTERM)" $ run "/tmp/echo-server" $ simpleAccessAnd sigTERM
    it "quit (SIGQUIT) while access" $ run "/tmp/echo-server" quitWhileAccess
    it "stop (SIGINT)  while access" $ run "/tmp/echo-server" $ stopWhileAccess sigINT
    it "stop (SIGTERM) while access" $ run "/tmp/echo-server" $ stopWhileAccess sigTERM
    it "restart (SIGHUP)" $ run "/tmp/echo-server" restartWhileAccess

removeFileIfExist :: FilePath -> IO ()
removeFileIfExist file = do
  exist <- doesFileExist file
  when exist $ removeFile file

waitStandby :: FilePath -> IO ()
waitStandby path =
    tryIO (readFile path) >>=
    either (\_ -> waitStandby path) (\_ -> threadDelay 1000)

removeFiles :: FilePath -> IO ()
removeFiles file =
    mapM_ (removeFileIfExist . (file ++)) [ ".sock", ".pid" ]

run :: String -> IO () -> IO ()
run file action = do
  removeFiles file
  bracket (rawSystem file []) (\_ -> return ()) $ \code -> do
    code `shouldBe` ExitSuccess
    waitStandby $ file ++ ".pid"
    action

kill :: Signal -> IO ()
kill signal = readFile "/tmp/echo-server.pid" >>=
              signalProcess signal . read

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

echo :: Socket -> String -> IO String
echo sock str = send sock str >> recv sock (2 * length str)

shouldEcho :: Socket -> String -> Expectation
shouldEcho sock str = echo sock str `shouldReturn` str

simpleAccess :: IO ()
simpleAccess = access (`shouldEcho` "simpleAccess")

access :: (Socket -> IO ()) -> IO ()
access action =
    bracket (socket AF_INET Stream 0) close $ \sock -> do
      addr <- inet_addr "127.0.0.1"
      connect sock $ SockAddrInet 8080 addr
      action sock

buildAsEchoServer :: FilePath -> IO ()
buildAsEchoServer file = rawSystem "ghc" [ "--make", file, "-o", "/tmp/echo-server" ] `shouldReturn` ExitSuccess

simpleAccessAnd :: Signal -> IO ()
simpleAccessAnd s = simpleAccess >> kill s

quitWhileAccess :: IO ()
quitWhileAccess = do
  res <- tryIO $ access $ \sock -> do
           kill sigQUIT
           replicateM_ 100 $ do
             sock `shouldEcho` "quitWhileAccess"
             threadDelay 1000
  res `shouldBe` Right ()

stopWhileAccess :: Signal -> IO ()
stopWhileAccess s = do
  res <- tryIO $ access $ \sock -> do
           kill s
           replicateM_ 100 $ do
             sock `shouldEcho` "quitWhileAccess"
             threadDelay 1000
  either (\_ -> True) (\_ -> False) res `shouldBe` True

restartWhileAccess :: IO ()
restartWhileAccess = do
  void $ forkIO $ threadDelay 10000 >> kill sigHUP
  replicateM_ 100 $ do
    res <- tryIO $ access $ \sock -> do
             sock `shouldEcho` "restart"
             threadDelay 1000
    res `shouldBe` Right ()
  kill sigQUIT