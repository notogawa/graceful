import Network
import Network.Socket ( send )
import Control.Concurrent
import Control.Monad
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
                 , gracefulSettingsSockFile = "/tmp/sample.sock"
                 , gracefulSettingsPidFile = "/tmp/sample.pid"
                 , gracefulSettingsBinary = "/tmp/sample"
                 }
      worker = GracefulWorker { gracefulWorkerInitialize = return ()
                              , gracefulWorkerApplication = application
                              , gracefulWorkerFinalize = const $ return ()
                              }
      application sock _ = do
        pid <- getProcessID
        let content = shows pid "\n"
        mapM_ (send sock)
                  [ "HTTP/1.1 200 OK\r\n"
                  , "Connection: close\r\n"
                  , "Content-Type: text/plain; charset=utf-8\r\n"
                  , "Content-Length: " ++ show (length content) ++ "\r\n"
                  , "\r\n"
                  , content
                  ]

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
