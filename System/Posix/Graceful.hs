module System.Posix.Graceful
    ( GracefulSettings(..)
    , graceful
    ) where

import Control.Monad ( replicateM, void, forever, when )
import Control.Concurrent ( newEmptyMVar, putMVar, takeMVar, forkIO, threadDelay )
import Control.Concurrent.STM ( atomically, newTVarIO, modifyTVar', readTVar )
import Network ( Socket, listenOn, PortID(..), PortNumber )
import Network.Socket ( Socket(..), socket, mkSocket
                      , connect, close, accept, shutdown, bindSocket, listen
                      , send, recv, sendFd, recvFd, fdSocket, SocketStatus(..)
                      , Family(..), SocketType(..), ShutdownCmd(..), SockAddr(..) )
import System.Posix.Signals ( Signal, signalProcess
                            , Handler(..), installHandler
                            , keyboardTermination, lostConnection
                            , keyboardSignal, softwareTermination
                            , userDefinedSignal2 )
import System.Posix.Process ( getProcessStatus, getProcessID
                            , forkProcess, exitImmediately, executeFile )
import System.Posix.Types ( ProcessID )
import Control.Exception ( IOException, bracket, bracket_, finally, try )
import System.Exit ( ExitCode(..) )
import System.Directory ( doesFileExist, removeFile )

-- | Server settings
data GracefulSettings resource =
    GracefulSettings { gracefulSettingsPortNumber :: PortNumber
                     , gracefulSettingsWorkerCount :: Int
                     , gracefulSettingsInitialize :: IO resource
                     , gracefulSettingsApplication :: Socket -> resource -> IO ()
                     , gracefulSettingsFinalize :: resource -> IO ()
                     , gracefulSettingsSockFile :: FilePath
                     , gracefulSettingsPidFile :: FilePath
                     , gracefulSettingsBinary :: FilePath
                     }

-- | Make server application enable shutdown/restart gracefully
graceful :: GracefulSettings a -> IO ()
graceful settings = do
  writeProcessId settings
  esock <- tryRecvSocket settings
  sock <- either (const $ listenPort settings) return esock
  let worker = workerProcess settings sock
      launch = launchWorkers (gracefulSettingsWorkerCount settings) worker
  pids <- launch
  quit <- newEmptyMVar
  resetHandlers HandlerSettings { handlerSettingsProcessIDs = pids
                                , handlerSettingsQuitProcess = putMVar quit True
                                , handlerSettingsLaunchWorkers = launch
                                , handlerSettingsSpawnProcess = spawnProcess settings sock
                                }
  void $ takeMVar quit

listenPort :: GracefulSettings resource -> IO Socket
listenPort = listenOn . PortNumber . gracefulSettingsPortNumber

tryRecvSocket :: GracefulSettings resource -> IO (Either IOException Socket)
tryRecvSocket settings =
    tryIO $ bracket (socket AF_UNIX Stream 0) close $ \uds -> do
      connect uds $ SockAddrUnix $ gracefulSettingsSockFile settings
      sock <- recvSock uds
      shutdown uds ShutdownBoth
      return sock

writeProcessId :: GracefulSettings resource -> IO ()
writeProcessId settings =
    getProcessID >>=
    writeFile (gracefulSettingsPidFile settings) . show

clearUnixDomainSocket :: FilePath -> IO ()
clearUnixDomainSocket sockFile = do
  exist <- doesFileExist sockFile
  when exist $ removeFile sockFile

spawnProcess :: GracefulSettings resource -> Socket -> IO ()
spawnProcess GracefulSettings { gracefulSettingsSockFile = sockFile
                              , gracefulSettingsBinary = binary
                              } sock = do
  clearUnixDomainSocket sockFile
  bracket (socket AF_UNIX Stream 0) close $ \uds -> do
    bindSocket uds $ SockAddrUnix sockFile
    listen uds 1
    void $ forkProcess $ executeFile binary False [] Nothing
    bracket (accept uds) (close . fst) $ \(s, _) -> do
      sendSock s sock
      shutdown s ShutdownBoth
    shutdown uds ShutdownBoth

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

sendSock :: Socket -> Socket -> IO ()
sendSock uds sock = do
  sendFd uds $ fdSocket sock
  let MkSocket _ family socktype protocol _ = sock
  void $ send uds $ show (family, socktype, protocol)

recvSock :: Socket -> IO Socket
recvSock uds = do
  fd <- recvFd uds
  (family, socktype, protocol) <- fmap read $ recv uds 2048
  mkSocket fd family socktype protocol Listening

broadcastSignal :: Signal -> [ProcessID] -> IO ()
broadcastSignal = mapM_ . signalProcess

waitAllProcess :: [ProcessID] -> IO ()
waitAllProcess = mapM_ $ getProcessStatus True True

shutdownGracefully :: [ProcessID] -> IO ()
shutdownGracefully pids = do
  broadcastSignal keyboardTermination pids
  waitAllProcess pids

launchWorkers :: Int -> IO () -> IO [ProcessID]
launchWorkers n = replicateM n . forkProcess

data HandlerSettings =
    HandlerSettings { handlerSettingsProcessIDs :: [ProcessID]
                    , handlerSettingsQuitProcess:: IO ()
                    , handlerSettingsLaunchWorkers :: IO [ProcessID]
                    , handlerSettingsSpawnProcess :: IO ()
                    }

resetHandlers :: HandlerSettings -> IO ()
resetHandlers settings = do
  void $ installHandler keyboardTermination (CatchOnce $ handleSIGQUIT settings) Nothing
  void $ installHandler lostConnection (CatchOnce $ handleSIGHUP settings) Nothing
  void $ installHandler keyboardSignal (CatchOnce $ handleSIGINT settings) Nothing
  void $ installHandler softwareTermination (CatchOnce $ handleSIGTERM settings) Nothing
  void $ installHandler userDefinedSignal2 (CatchOnce $ handleSIGUSR2 settings) Nothing

defaultHandlers :: IO ()
defaultHandlers = do
  void $ installHandler keyboardTermination Default Nothing
  void $ installHandler lostConnection Default Nothing
  void $ installHandler keyboardSignal Default Nothing
  void $ installHandler softwareTermination Default Nothing
  void $ installHandler userDefinedSignal2 Default Nothing

-- fast shutdown
handleSIGINT :: HandlerSettings -> IO ()
handleSIGINT settings = do
  broadcastSignal keyboardSignal $ handlerSettingsProcessIDs settings
  exitImmediately $ ExitFailure 130 -- SIGINT exit code

-- fast shutdown
handleSIGTERM :: HandlerSettings -> IO ()
handleSIGTERM settings = do
  broadcastSignal softwareTermination $ handlerSettingsProcessIDs settings
  exitImmediately $ ExitFailure 143 -- SIGTERM exit code

-- graceful shutdown
handleSIGQUIT :: HandlerSettings -> IO ()
handleSIGQUIT settings = do
  shutdownGracefully $ handlerSettingsProcessIDs settings
  handlerSettingsQuitProcess settings

-- starting new worker processes, graceful shutdown of old worker processes
handleSIGHUP :: HandlerSettings -> IO ()
handleSIGHUP settings = do
  newpids <- handlerSettingsLaunchWorkers settings
  resetHandlers settings { handlerSettingsProcessIDs = newpids }
  shutdownGracefully $ handlerSettingsProcessIDs settings

handleSIGUSR2 :: HandlerSettings -> IO ()
handleSIGUSR2 settings = do
  handlerSettingsSpawnProcess settings
  shutdownGracefully $ handlerSettingsProcessIDs settings
  handlerSettingsQuitProcess settings

workerProcess :: GracefulSettings resource -> Socket -> IO ()
workerProcess GracefulSettings { gracefulSettingsInitialize = initialize
                               , gracefulSettingsApplication = application
                               , gracefulSettingsFinalize = finalize
                               } sock = do
  defaultHandlers
  void $ installHandler keyboardTermination (CatchOnce $ close sock) Nothing
  count <- newTVarIO (0 :: Int)
  void $ tryIO $ bracket initialize finalize $ \resource ->
      void $ forever $ do
        (s, _) <- accept sock
        let app = application s resource >> shutdown s ShutdownBoth
        forkIO $ bracket_
                   (atomically $ modifyTVar' count succ)
                   (atomically $ modifyTVar' count pred)
                   (app `finally` close s)
  waitAllAction count
  close sock
  exitImmediately ExitSuccess
  where
    waitAllAction count = do
      active <- atomically $ readTVar count
      when (0 /= active) $ do
        threadDelay 1000
        waitAllAction count
