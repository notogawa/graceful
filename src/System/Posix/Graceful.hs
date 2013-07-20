{-# LANGUAGE CPP #-}
module System.Posix.Graceful
    ( GracefulSettings(..)
    , graceful
    ) where

import Control.Concurrent ( newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM ( newTVarIO )
import Control.Exception ( IOException, bracket, bracket_, try )
import Control.Monad ( replicateM, void, when )
import Network ( Socket, listenOn, PortID(..), PortNumber )
#if MIN_VERSION_network(2,4,0)
import Network.Socket ( close )
#else
import Network.Socket ( sClose )
#endif
import Network.Socket ( Socket(..), socket, mkSocket
                      , connect, accept, shutdown, bindSocket, listen
                      , send, recv, sendFd, recvFd, fdSocket, SocketStatus(..)
                      , Family(..), SocketType(..), ShutdownCmd(..), SockAddr(..) )
import System.Directory ( doesFileExist, removeFile )
import System.Posix.Process ( getProcessID, forkProcess, executeFile )
import System.Posix.Signals ( blockSignals, unblockSignals, fullSignalSet )
import System.Posix.Types ( ProcessID )

import System.Posix.Graceful.Handler
import System.Posix.Graceful.Worker

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

toWorkerSettings :: GracefulSettings resource -> WorkerSettings resource
toWorkerSettings settings =
    WorkerSettings { workerSettingsInitialize = gracefulSettingsInitialize settings
                   , workerSettingsApplication = gracefulSettingsApplication settings
                   , workerSettingsFinalize = gracefulSettingsFinalize settings
                   }

-- | Make server application enable shutdown/restart gracefully
graceful :: GracefulSettings a -> IO ()
graceful settings = do
  quit <- newEmptyMVar
  bracket_ (blockSignals fullSignalSet) (unblockSignals fullSignalSet) $ do
    esock <- tryRecvSocket settings
    sock <- either (const $ listenPort settings) return esock
    let worker = defaultHandlers >> workerProcess (toWorkerSettings settings) sock
        launch = launchWorkers (gracefulSettingsWorkerCount settings) $ do
                   unblockSignals fullSignalSet
                   worker
    pids <- launch >>= newTVarIO
    resetHandlers HandlerSettings { handlerSettingsProcessIDs = pids
                                  , handlerSettingsQuitProcess = putMVar quit True
                                  , handlerSettingsLaunchWorkers = launch
                                  , handlerSettingsSpawnProcess = spawnProcess settings sock
                                  }
    writeProcessId settings
  void $ takeMVar quit

listenPort :: GracefulSettings resource -> IO Socket
listenPort = listenOn . PortNumber . gracefulSettingsPortNumber

tryRecvSocket :: GracefulSettings resource -> IO (Either IOException Socket)
tryRecvSocket settings =
    tryIO $ bracket (socket AF_UNIX Stream 0) wrapClose $ \uds -> do
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
  bracket (socket AF_UNIX Stream 0) wrapClose $ \uds -> do
    bindSocket uds $ SockAddrUnix sockFile
    listen uds 1
    void $ forkProcess $ executeFile binary False [] Nothing
    bracket (accept uds) (wrapClose . fst) $ \(s, _) -> do
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

launchWorkers :: Int -> IO () -> IO [ProcessID]
launchWorkers n = replicateM n . forkProcess

wrapClose :: Socket -> IO ()
#if MIN_VERSION_network(2,4,0)
wrapClose = close
#else
wrapClose = sClose
#endif
