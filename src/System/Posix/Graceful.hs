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
    , GracefulWorker(..)
    , graceful
    ) where

import Control.Concurrent ( newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM ( newTVarIO )
import Control.Exception ( IOException, bracket, bracket_, try, throwIO )
import Control.Monad ( replicateM, void, when )
import Network.Socket.Wrapper ( Socket(..), socket, mkSocket
                              , connect, close, accept, bindSocket, listen
                              , send, recv, sendFd, recvFd, fdSocket, SocketStatus(..)
                              , Family(..), SocketType(..), SockAddr(..) )
import System.Directory ( doesFileExist, removeFile, renameFile )
import System.Environment ( getArgs )
import System.Posix.IO ( dup )
import System.Posix.Process ( getProcessID, forkProcess, executeFile, getProcessStatus )
import System.Posix.Signals ( blockSignals, unblockSignals, fullSignalSet )
import System.Posix.Types ( ProcessID, Fd(Fd) )

import System.Posix.Graceful.Handler
import System.Posix.Graceful.Worker

-- | Server settings
--
-- Since 0.1.0.0
--
data GracefulSettings =
    GracefulSettings
    { gracefulSettingsListen :: IO Socket -- ^ Listen socket
    , gracefulSettingsWorkerCount :: Int -- ^ Prefork worker count
    , gracefulSettingsSockFile :: FilePath -- ^ Unix domain socket file
    , gracefulSettingsPidFile :: FilePath -- ^ The file to which the server records the process id
    , gracefulSettingsBinary :: FilePath -- ^ The binary file to upgrade
    }

-- | Make server application enable shutdown/restart gracefully
--
-- Since 0.1.0.0
--
graceful :: GracefulSettings -> GracefulWorker -> IO ()
graceful settings worker = do
  quit <- newEmptyMVar
  result <- tryIO $ bracket_ (blockSignals fullSignalSet) (unblockSignals fullSignalSet) $ do
    esock <- tryRecvSocket settings
    sock <- either (const $ gracefulSettingsListen settings) return esock
    let launch = launchWorkers (gracefulSettingsWorkerCount settings) $ do
                   unblockSignals fullSignalSet
                   defaultHandlers
                   workerProcess worker sock
    pids <- launch >>= newTVarIO
    resetHandlers HandlerSettings { handlerSettingsProcessIDs = pids
                                  , handlerSettingsQuitProcess = putMVar quit True
                                  , handlerSettingsLaunchWorkers = launch
                                  , handlerSettingsSpawnProcess = spawnProcess settings sock
                                  }
  writeProcessId settings
  either throwIO (const $ void $ takeMVar quit) result

tryRecvSocket :: GracefulSettings -> IO (Either IOException Socket)
tryRecvSocket settings =
    tryIO $ bracket (socket AF_UNIX Stream 0) close $ \uds -> do
      connect uds $ SockAddrUnix $ gracefulSettingsSockFile settings
      recvSock uds

writeProcessId :: GracefulSettings -> IO ()
writeProcessId settings =
    getProcessID >>= writeFile (gracefulSettingsPidFile settings) . show

clearUnixDomainSocket :: FilePath -> IO ()
clearUnixDomainSocket sockFile = do
  exist <- doesFileExist sockFile
  when exist $ removeFile sockFile

spawnProcess :: GracefulSettings -> Socket -> IO ()
spawnProcess GracefulSettings { gracefulSettingsSockFile = sockFile
                              , gracefulSettingsBinary = binary
                              , gracefulSettingsPidFile = pidFile
                              } sock = do
  exist <- doesFileExist pidFile
  when exist $ do
    clearUnixDomainSocket sockFile
    bracket (socket AF_UNIX Stream 0) close $ \uds -> do
      bindSocket uds $ SockAddrUnix sockFile
      listen uds 1
      args <- getArgs
      pid <- forkProcess $ executeFile binary False args Nothing
      bracket (accept uds) (close . fst) $ \(s, _) -> sendSock s sock
      renameFile pidFile (pidFile ++ ".old")
      void $ getProcessStatus True False pid

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

sendSock :: Socket -> Socket -> IO ()
sendSock uds sock = do
  Fd fd <- dup $ Fd $ fdSocket sock
  sendFd uds fd
  let MkSocket _ family socktype protocol _ = sock
  void $ send uds $ show (family, socktype, protocol)

recvSock :: Socket -> IO Socket
recvSock uds = do
  fd <- recvFd uds
  (family, socktype, protocol) <- fmap read $ recv uds 2048
  mkSocket fd family socktype protocol Listening

launchWorkers :: Int -> IO () -> IO [ProcessID]
launchWorkers n = replicateM n . forkProcess
