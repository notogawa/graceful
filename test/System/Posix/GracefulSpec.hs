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
import System.Process hiding ( cwd )
import Test.Hspec

spec :: Spec
spec = describe "graceful" $ do
         it "simple access and quit (SIGQUIT)" $ run $ simpleAccessAnd sigQUIT
         it "simple access and stop (SIGINT)"  $ run $ simpleAccessAnd sigINT
         it "simple access and stop (SIGTERM)" $ run $ simpleAccessAnd sigTERM
         it "quit (SIGQUIT) while access" $ run quitWhileAccess
         it "stop (SIGINT)  while access" $ run $ stopWhileAccess sigINT
         it "stop (SIGTERM) while access" $ run $ stopWhileAccess sigTERM
         it "restart (SIGHUP)" $ run restartWhileAccess
         it "upgrade (SIGUSR2)" $ run upgradeWhileAccess

removeFileIfExist :: FilePath -> IO ()
removeFileIfExist file = do
  exist <- doesFileExist file
  when exist $ removeFile file

waitStandby :: FilePath -> IO ()
waitStandby path = do
  status <- tryIO $ readFile path
  case status of
    Left _err -> threadDelay 1000 >> waitStandby path
    Right _ok -> return ()

run :: IO () -> IO ()
run action = do
  buildAsEchoServer "test/echo.hs"
  cwd <- getCurrentDirectory
  let file = cwd ++ "/tmp/echo-server"
  mapM_ (removeFileIfExist . (file ++)) [ ".sock", ".pid" ]
  rawSystem file [] `shouldReturn` ExitSuccess
  waitStandby $ file ++ ".pid"
  action

kill :: Signal -> IO ()
kill signal = getCurrentDirectory >>=
              readFile . (++ "/tmp/echo-server.pid") >>=
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


packageOption :: String
#if __GLASGOW_HASKELL__ < 706
packageOption = "-package-conf"
#else
packageOption = "-package-db"
#endif

buildAsEchoServer :: FilePath -> IO ()
buildAsEchoServer file = do
  cwd <- getCurrentDirectory
  removeFileIfExist (cwd ++ "/tmp/echo-server")
  (code, _out, _err) <- readProcessWithExitCode "ghc"
                        [ "--make", file
                        , "-o", cwd ++ "/tmp/echo-server"
                        , packageOption, "dist/package.conf.inplace"
                        ] ""
  code `shouldBe` ExitSuccess

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
            replicateM_ 100 $ do
              sock `shouldEcho` "upgradeWhileAccess"
              threadDelay 1000
  res1 `shouldBe` Right ()
  threadDelay 1000000
  res2 <- tryIO $ access $ \sock ->
            replicateM_ 100 $ do
              sock `shouldDouble` "upgradeWhileAccess"
              threadDelay 1000
  res2 `shouldBe` Right ()
  kill sigQUIT
