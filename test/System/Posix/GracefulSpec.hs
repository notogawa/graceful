{-# LANGUAGE CPP #-}
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
spec = describe "graceful" $ do
         it "build echo as echo" $ buildAsEchoServer "test/echo.hs"
         it "simple access and quit (SIGQUIT)" $ run "/tmp/echo-server" $ simpleAccessAnd sigQUIT
         it "simple access and stop (SIGINT)"  $ run "/tmp/echo-server" $ simpleAccessAnd sigINT
         it "simple access and stop (SIGTERM)" $ run "/tmp/echo-server" $ simpleAccessAnd sigTERM
         it "quit (SIGQUIT) while access" $ run "/tmp/echo-server" quitWhileAccess
         it "stop (SIGINT)  while access" $ run "/tmp/echo-server" $ stopWhileAccess sigINT
         it "stop (SIGTERM) while access" $ run "/tmp/echo-server" $ stopWhileAccess sigTERM
         it "restart (SIGHUP)" $ run "/tmp/echo-server" restartWhileAccess
         it "upgrade (SIGUSR2)" $ run "/tmp/echo-server" upgradeWhileAccess

removeFileIfExist :: FilePath -> IO ()
removeFileIfExist file = do
  exist <- doesFileExist file
  when exist $ removeFile file

waitStandby :: FilePath -> IO ()
waitStandby path = do
  status <- tryIO $ readFile path
  case status of
    Left _err -> waitStandby path
    Right _ok -> threadDelay 1000

run :: String -> IO () -> IO ()
run file action = do
  mapM_ (removeFileIfExist . (file ++)) [ ".sock", ".pid" ]
  rawSystem file [] `shouldReturn` ExitSuccess
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

shouldDouble :: Socket -> String -> Expectation
shouldDouble sock str = echo sock str `shouldReturn` (str ++ str)

simpleAccess :: IO ()
simpleAccess = access (`shouldEcho` "simpleAccess")

wrapClose :: Socket -> IO ()
#if MIN_VERSION_network(2,4,0)
wrapClose = close
#else
wrapClose = sClose
#endif

access :: (Socket -> IO ()) -> IO ()
access action =
    bracket (socket AF_INET Stream 0) wrapClose $ \sock -> do
      addr <- inet_addr "127.0.0.1"
      connect sock $ SockAddrInet 8080 addr
      action sock

buildAsEchoServer :: FilePath -> IO ()
buildAsEchoServer file = do
  removeFileIfExist "/tmp/echo-server"
#if __GLASGOW_HASKELL__ < 706
  rawSystem "ghc" [ "--make", file, "-o", "/tmp/echo-server", "-package-conf", "dist/package.conf.inplace" ] `shouldReturn` ExitSuccess
#else
  rawSystem "ghc" [ "--make", file, "-o", "/tmp/echo-server", "-package-db", "dist/package.conf.inplace" ] `shouldReturn` ExitSuccess
#endif
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
  either (const True) (const False) res `shouldBe` True

restartWhileAccess :: IO ()
restartWhileAccess = do
  res1 <- tryIO $ access $ \sock -> do
            kill sigHUP
            sock `shouldEcho` "restart"
  res1 `shouldBe` Right ()
  res2 <- tryIO $ access (`shouldEcho` "restart")
  res2 `shouldBe` Right ()
  kill sigQUIT

upgradeWhileAccess :: IO ()
upgradeWhileAccess = do
  buildAsEchoServer "test/double.hs"
  res1 <- tryIO $ access $ \sock -> do
            kill sigUSR2
            sock `shouldEcho` "restart"
  res1 `shouldBe` Right ()
  threadDelay 1000000
  res2 <- tryIO $ access (`shouldDouble` "restart")
  res2 `shouldBe` Right ()
  kill sigQUIT
