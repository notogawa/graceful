{-# LANGUAGE CPP #-}
module System.Posix.GracefulSpec ( spec ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Network
import Network.Socket
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath
import System.Posix.Files
import System.Posix.Signals
import System.Posix.Types
import System.Process
import Test.Hspec

spec :: Spec
spec = describe "graceful" $ do
         it "prefork workers" $ run preforkWorkers
         it "restart keep workers > 0" $ run restartKeepWorkers
         it "upgrade keep workers > 0" $ run upgradeKeepWorkers
         it "abort upgrade keep workers > 0" $ run abortUpgradeKeepWorkers
         it "simple access and quit (SIGQUIT)" $ run $ simpleAccessAnd sigQUIT
         it "simple access and stop (SIGINT)"  $ run $ simpleAccessAnd sigINT
         it "simple access and stop (SIGTERM)" $ run $ simpleAccessAnd sigTERM
         it "quit (SIGQUIT) while access" $ run quitWhileAccess
         it "stop (SIGINT)  while access" $ run $ stopWhileAccess sigINT
         it "stop (SIGTERM) while access" $ run $ stopWhileAccess sigTERM
         it "restart (SIGHUP) while access" $ run restartWhileAccess
         it "upgrade (SIGUSR2) while access" $ run upgradeWhileAccess
         it "abort upgrade while access" $ run abortUpgradeWhileAccess

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

waitProcessIncreaseTo :: Int -> IO ()
waitProcessIncreaseTo n = do
  procs <- fmap length ps
  procs `shouldSatisfy` (<= n)
  if procs < n
    then threadDelay 1000 >> waitProcessIncreaseTo n
    else procs `shouldBe` n

waitProcessDecreaseTo :: Int -> IO ()
waitProcessDecreaseTo n = do
  procs <- fmap length ps
  procs `shouldSatisfy` (>= n)
  if procs > n
    then threadDelay 1000 >> waitProcessDecreaseTo n
    else procs `shouldBe` n

run :: IO () -> IO ()
run action = do
  buildAsEchoServer "test/echo.hs"
  let file = "/tmp/echo-server"
  mapM_ (removeFileIfExist . (file ++)) [ ".sock", ".pid" ]
  rawSystem file [] `shouldReturn` ExitSuccess
  waitStandby $ file ++ ".pid"
  action
  waitProcessDecreaseTo 0

kill :: Signal -> IO ()
kill signal = readFile "/tmp/echo-server.pid" >>=
              signalProcess signal . read

killold :: Signal -> IO ()
killold signal = readFile "/tmp/echo-server.pid.old" >>=
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
  removeFileIfExist "/tmp/echo-server"
  confDistDir <- getConfDistDir
  (code, _out, _err) <- readProcessWithExitCode "ghc"
                        [ "--make", file
                        , "-o", "/tmp/echo-server"
                        , packageOption, confDistDir ++ "/package.conf.inplace"
                        ] ""
  code `shouldBe` ExitSuccess

getConfDistDir :: IO FilePath
getConfDistDir = fmap (dirname . dirname . dirname) getModuleFile where
    dirname = takeDirectory
    getModuleFile = readSymbolicLink "/proc/self/exe"

ps :: IO [ProcessID]
ps = do
  (_code, out, _err) <- readProcessWithExitCode "ps"
                        [ "hopid", "-Cecho-server" ] ""
  return $ map read $ words out

simpleAccessAnd :: Signal -> IO ()
simpleAccessAnd s = simpleAccess >> kill s

preforkWorkers :: IO ()
preforkWorkers = do
  fmap length ps `shouldReturn` 5
  kill sigQUIT

restartKeepWorkers :: IO ()
restartKeepWorkers = do
  pids <- ps
  length pids `shouldBe` 5 -- master + 4 worker
  kill sigHUP
  waitProcessDecreaseTo 5
  pids' <- ps
  length pids' `shouldBe` 5 -- master + 4 worker
  length (pids `intersect` pids') `shouldBe` 1 -- restarted workers
  kill sigQUIT

upgradeKeepWorkers :: IO ()
upgradeKeepWorkers = do
  pids <- ps
  length pids `shouldBe` 5 -- master + 4 worker
  kill sigUSR2
  waitProcessIncreaseTo 10
  killold sigQUIT
  waitProcessDecreaseTo 5
  pids' <- ps
  length pids' `shouldBe` 5 -- master + 4 worker
  length (pids `intersect` pids') `shouldBe` 0 -- upgraded master & workers
  kill sigQUIT

abortUpgradeKeepWorkers :: IO ()
abortUpgradeKeepWorkers = do
  pids <- ps
  length pids `shouldBe` 5 -- master + 4 worker
  kill sigUSR2
  waitProcessIncreaseTo 10
  kill sigQUIT
  renameFile "/tmp/echo-server.pid.old" "/tmp/echo-server.pid"
  waitProcessDecreaseTo 5
  pids' <- ps
  length pids' `shouldBe` 5 -- master + 4 worker
  length (pids `intersect` pids') `shouldBe` 5 -- abort upgrade
  kill sigQUIT

left :: Either a b -> Bool
left = either (const True) (const False)

right :: Either a b -> Bool
right = not . left

quitWhileAccess :: IO ()
quitWhileAccess = do
  res <- tryIO $ access $ \sock -> do
           kill sigQUIT
           replicateM_ 100 $ do
             sock `shouldEcho` "quitWhileAccess"
             threadDelay 1000
  res `shouldSatisfy` right

stopWhileAccess :: Signal -> IO ()
stopWhileAccess s = do
  res <- tryIO $ access $ \sock -> do
           kill s
           replicateM_ 100 $ do
             sock `shouldEcho` "stopWhileAccess"
             threadDelay 1000
  res `shouldSatisfy` left

restartWhileAccess :: IO ()
restartWhileAccess = do
  access $ \sock -> do
    kill sigHUP
    replicateM_ 10 $ do
      sock `shouldEcho` "restartWhileAccess"
      threadDelay 1000
  waitProcessDecreaseTo 5
  access $ \sock ->
      replicateM_ 10 $ do
        sock `shouldEcho` "restartWhileAccess"
        threadDelay 1000
  kill sigQUIT

upgradeWhileAccess :: IO ()
upgradeWhileAccess = do
  buildAsEchoServer "test/double.hs"
  access $ \sock -> do
    kill sigUSR2
    replicateM_ 10 $ do
      sock `shouldEcho` "upgradeWhileAccess"
      threadDelay 1000
  waitProcessIncreaseTo 10
  killold sigQUIT
  waitProcessDecreaseTo 5
  access $ \sock ->
      replicateM_ 10 $ do
        sock `shouldDouble` "upgradeWhileAccess"
        threadDelay 1000
  kill sigQUIT

abortUpgradeWhileAccess :: IO ()
abortUpgradeWhileAccess = do
  buildAsEchoServer "test/double.hs"
  access $ \sock -> do
    kill sigUSR2
    replicateM_ 10 $ do
      sock `shouldEcho` "upgradeWhileAccess"
      threadDelay 1000
  waitProcessIncreaseTo 10
  kill sigQUIT
  renameFile "/tmp/echo-server.pid.old" "/tmp/echo-server.pid"
  waitProcessDecreaseTo 5
  access $ \sock ->
      replicateM_ 10 $ do
        sock `shouldEcho` "upgradeWhileAccess"
        threadDelay 1000
  kill sigQUIT
