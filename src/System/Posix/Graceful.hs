-- |
-- Module      : System.Posix.Graceful
-- Copyright   : 2013 Noriyuki OHKAWA
-- License     : BSD3
--
-- Maintainer  : n.ohkawa@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Provides function to make process graceful.
module System.Posix.Graceful
    ( GracefulSettings(..)
    , graceful
    ) where

import Control.Concurrent ( newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM ( newTVarIO )
import Control.Exception ( IOException, bracket, bracket_, try, throwIO )
import Control.Monad ( replicateM, void, when )
import Network ( Socket, listenOn, PortID(..), PortNumber )
import Network.Socket.Wrapper ( Socket(..), socket, mkSocket
                              , connect, close, accept, shutdown, bindSocket, listen
                              , send, recv, sendFd, recvFd, fdSocket, SocketStatus(..)
                              , Family(..), SocketType(..), ShutdownCmd(..), SockAddr(..) )
import System.Directory ( doesFileExist, removeFile, renameFile )
import System.Posix.Process ( getProcessID, forkProcess, executeFile )
import System.Posix.Signals ( blockSignals, unblockSignals, fullSignalSet )
import System.Posix.Types ( ProcessID )

import System.Posix.Graceful.Handler
import System.Posix.Graceful.Worker

-- | Server settings
data GracefulSettings resource =
    GracefulSettings { gracefulSettingsPortNumber :: PortNumber -- ^ Listen port
                     , gracefulSettingsWorkerCount :: Int -- ^ Prefork worker count
                     , gracefulSettingsInitialize :: IO resource -- ^ Worker initializer to initialize user defined resource
                     , gracefulSettingsApplication :: Socket -> resource -> IO () -- ^ Worker action
                     , gracefulSettingsFinalize :: resource -> IO () -- ^ Worker finalizer to finalize user defined resource
                     , gracefulSettingsSockFile :: FilePath -- ^ Unix domain socket file
                     , gracefulSettingsPidFile :: FilePath -- ^ The file to which the server records the process id
                     , gracefulSettingsBinary :: FilePath -- ^ The binary file to upgrade
                     }

toWorkerSettings :: GracefulSettings resource -> WorkerSettings resource
toWorkerSettings settings =
    WorkerSettings { workerSettingsInitialize = gracefulSettingsInitialize settings
                   , workerSettingsApplication = gracefulSettingsApplication settings
                   , workerSettingsFinalize = gracefulSettingsFinalize settings
                   }

-- | Make server application enable shutdown/restart gracefully
graceful :: GracefulSettings resource -> IO ()
graceful settings = do
  quit <- newEmptyMVar
  result <- tryIO $ bracket_ (blockSignals fullSignalSet) (unblockSignals fullSignalSet) $ do
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
  either throwIO (const $ void $ takeMVar quit) result

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
    getProcessID >>= writeFile (gracefulSettingsPidFile settings) . show

clearUnixDomainSocket :: FilePath -> IO ()
clearUnixDomainSocket sockFile = do
  exist <- doesFileExist sockFile
  when exist $ removeFile sockFile

spawnProcess :: GracefulSettings resource -> Socket -> IO ()
spawnProcess GracefulSettings { gracefulSettingsSockFile = sockFile
                              , gracefulSettingsBinary = binary
                              , gracefulSettingsPidFile = pidFile
                              } sock = do
  exist <- doesFileExist pidFile
  when exist $ renameFile pidFile (pidFile ++ ".old")
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

launchWorkers :: Int -> IO () -> IO [ProcessID]
launchWorkers n = replicateM n . forkProcess
