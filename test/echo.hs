import Network
import Network.Socket ( send, recv )
import Control.Concurrent
import Control.Monad
import System.Directory
import System.Exit
import System.Posix.Directory
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals

import System.Posix.Graceful

main :: IO ()
main = daemonize $ graceful settings worker
    where
      settings = GracefulSettings
                 { gracefulSettingsListen = listenOn $ PortNumber 8080
                 , gracefulSettingsWorkerCount = 4
                 , gracefulSettingsSockFile = "/tmp/echo-server.sock"
                 , gracefulSettingsPidFile = "/tmp/echo-server.pid"
                 , gracefulSettingsBinary = "/tmp/echo-server"
                 }
      worker = GracefulWorker { gracefulWorkerInitialize = return ()
                              , gracefulWorkerApplication = application
                              , gracefulWorkerFinalize = const $ return ()
                              }
      application sock _ = forever $ recv sock 1024 >>= send sock

daemonize :: IO () -> IO ()
daemonize application = do
  void $ setFileCreationMask 0
  void $ forkProcess $ do
    void createSession
    void $ forkProcess $ do
      changeWorkingDirectory "/"
      devnull <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
      let sendTo fd' fd = closeFd fd >> dupTo fd' fd
      mapM_ (sendTo devnull) [ stdInput, stdOutput, stdError ]
      void $ installHandler sigHUP Ignore Nothing
      application
    exitImmediately ExitSuccess
  exitImmediately ExitSuccess
