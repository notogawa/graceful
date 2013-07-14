import Network.Socket
import System.Posix.Graceful
import System.Posix.Process
import Control.Concurrent

main :: IO ()
main = graceful settings
    where
      settings = GracefulSettings
                 { gracefulSettingsPortNumber = 8080
                 , gracefulSettingsWorkerCount = 4
                 , gracefulSettingsInitialize = return ()
                 , gracefulSettingsApplication = application
                 , gracefulSettingsFinalize = const $ return ()
                 , gracefulSettingsSockFile = "/tmp/sample.sock"
                 , gracefulSettingsPidFile = "/tmp/sample.pid"
                 , gracefulSettingsBinary = "./dist/build/sample/sample"
                 }
      application sock _ = do
        pid <- getProcessID
        threadDelay 1000000
        let content = shows pid " test\n"
        mapM_ (send sock)
                  [ "HTTP/1.1 200 OK\r\n"
                  , "Connection: close\r\n"
                  , "Content-Type: text/plain; charset=utf-8\r\n"
                  , "Content-Length: " ++ show (length content) ++ "\r\n"
                  , "\r\n"
                  , content
                  ]
