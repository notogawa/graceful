module System.Posix.Graceful
    ( GracefulSettings(..)
    , graceful
    ) where

import Control.Monad ( replicateM, void, forever, when )
import Control.Concurrent ( MVar, newEmptyMVar, putMVar, takeMVar, forkIO, threadDelay )
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
graceful GracefulSettings { gracefulSettingsPortNumber = portNumber
                          , gracefulSettingsWorkerCount = workerCount
                          , gracefulSettingsInitialize = initialize
                          , gracefulSettingsApplication = application
                          , gracefulSettingsFinalize = finalize
                          , gracefulSettingsSockFile = sockFile
                          , gracefulSettingsPidFile = pidFile
                          , gracefulSettingsBinary = binary
                          } = do
  esock <- tryIO $ bracket (socket AF_UNIX Stream 0) close $ \uds -> do
             connect uds (SockAddrUnix sockFile)
             sock <- recvSock uds
             shutdown uds ShutdownBoth
             return sock
  getProcessID >>= writeFile pidFile . show
  sock <- either (const $ listenOn $ PortNumber portNumber) return esock
  let launch = launchWorkers workerCount $ workerProcess sock initialize finalize application
      spawn = spawnProcess binary sock sockFile
  pids <- launch
  quit <- newEmptyMVar
  resetHandlers pids quit launch spawn
  void $ takeMVar quit
  return ()

clearUnixDomainSocket :: FilePath -> IO ()
clearUnixDomainSocket sockFile = do
  exist <- doesFileExist sockFile
  when exist $ removeFile sockFile

spawnProcess :: FilePath -> Socket -> FilePath -> IO ()
spawnProcess binary sock sockFile = do
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

resetHandlers :: [ProcessID] -> MVar Bool -> IO [ProcessID] -> IO () -> IO ()
resetHandlers pids quit launch spawn = do
  void $ installHandler keyboardTermination (CatchOnce $ handleSIGQUIT pids quit) Nothing
  void $ installHandler lostConnection (CatchOnce $ handleSIGHUP pids quit launch spawn) Nothing
  void $ installHandler keyboardSignal (CatchOnce $ handleSIGINT pids) Nothing
  void $ installHandler softwareTermination (CatchOnce $ handleSIGTERM pids) Nothing
  void $ installHandler userDefinedSignal2 (CatchOnce $ handleSIGUSR2 pids quit spawn) Nothing

defaultHandlers :: IO ()
defaultHandlers = do
  void $ installHandler keyboardTermination Default Nothing
  void $ installHandler lostConnection Default Nothing
  void $ installHandler keyboardSignal Default Nothing
  void $ installHandler softwareTermination Default Nothing
  void $ installHandler userDefinedSignal2 Default Nothing

-- fast shutdown
handleSIGINT :: [ProcessID] -> IO ()
handleSIGINT pids = do
  broadcastSignal keyboardSignal pids
  exitImmediately $ ExitFailure 130 -- SIGINT exit code

-- fast shutdown
handleSIGTERM :: [ProcessID] -> IO ()
handleSIGTERM pids = do
  broadcastSignal softwareTermination pids
  exitImmediately $ ExitFailure 143 -- SIGTERM exit code

-- graceful shutdown
handleSIGQUIT :: [ProcessID] -> MVar Bool -> IO ()
handleSIGQUIT pids quit = do
  shutdownGracefully pids
  putMVar quit True

-- starting new worker processes, graceful shutdown of old worker processes
handleSIGHUP :: [ProcessID] -> MVar Bool -> IO [ProcessID] -> IO () -> IO ()
handleSIGHUP oldpids quit launch spawn = do
  newpids <- launch
  resetHandlers newpids quit launch spawn
  shutdownGracefully oldpids

handleSIGUSR2 :: [ProcessID] -> MVar Bool -> IO () -> IO ()
handleSIGUSR2 pids quit spawn = do
  spawn
  shutdownGracefully pids
  putMVar quit True

workerProcess :: Socket
              -> IO resource
              -> (resource -> IO ())
              -> (Socket -> resource -> IO ())
              -> IO ()
workerProcess sock initialize finalize application = do
  defaultHandlers
  void $ installHandler keyboardTermination (CatchOnce $ close sock) Nothing
  count <- newTVarIO (0 :: Int)
  void $ tryIO $ bracket initialize finalize $ \resource -> do
    void $ forever $ do
      (s, _) <- accept sock
      let app = do
            void $ application s resource
            shutdown s ShutdownBoth
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
