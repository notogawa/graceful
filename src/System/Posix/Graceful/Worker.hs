{-# LANGUAGE CPP #-}
module System.Posix.Graceful.Worker
    ( WorkerSettings(..)
    , workerProcess
    ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM ( atomically, newTVarIO, modifyTVar', readTVar )
import Control.Exception ( IOException, bracket, bracket_, finally, try )
import Control.Monad ( void, forever, when )
import Network ( Socket )
#if MIN_VERSION_network(2,4,0)
import Network.Socket ( close )
#else
import Network.Socket ( sClose )
#endif
import Network.Socket ( accept, shutdown, ShutdownCmd(ShutdownBoth) )
import System.Exit ( ExitCode(..) )
import System.Posix.Process ( exitImmediately )
import System.Posix.Signals ( Handler(..), installHandler, sigQUIT )

data WorkerSettings resource =
    WorkerSettings { workerSettingsInitialize :: IO resource
                   , workerSettingsApplication :: Socket -> resource -> IO ()
                   , workerSettingsFinalize :: resource -> IO ()
                   }

tryIO :: IO a -> IO (Either IOException a)
tryIO = try


wrapClose :: Socket -> IO ()
#if MIN_VERSION_network(2,4,0)
wrapClose = close
#else
wrapClose = sClose
#endif

workerProcess :: WorkerSettings resource -> Socket -> IO ()
workerProcess WorkerSettings { workerSettingsInitialize = initialize
                             , workerSettingsApplication = application
                             , workerSettingsFinalize = finalize
                             } sock = do
  void $ installHandler sigQUIT (CatchOnce $ wrapClose sock) Nothing
  count <- newTVarIO (0 :: Int)
  void $ tryIO $ bracket initialize finalize $ \resource ->
      void $ forever $ do
        (s, _) <- accept sock
        let app = application s resource >> shutdown s ShutdownBoth
        forkIO $ bracket_
                   (atomically $ modifyTVar' count succ)
                   (atomically $ modifyTVar' count pred)
                   (app `finally` wrapClose s)
  waitAllAction count
  wrapClose sock
  exitImmediately ExitSuccess
  where
    waitAllAction count = do
      active <- atomically $ readTVar count
      when (0 /= active) $ do
        threadDelay 1000
        waitAllAction count
