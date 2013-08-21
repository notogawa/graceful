{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Module      : System.Posix.Graceful.Worker
-- Copyright   : 2013 Noriyuki OHKAWA
-- License     : BSD3
--
-- Maintainer  : n.ohkawa@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Worker process
module System.Posix.Graceful.Worker
    ( GracefulWorker(..)
    , workerProcess
    ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM ( atomically, newTVarIO, modifyTVar', readTVar )
import Control.Exception ( IOException, bracket, bracket_, finally, try )
import Control.Monad ( void, forever, when )
import Network ( Socket )
import Network.Socket.Wrapper ( close, accept )
import System.Exit ( ExitCode(..) )
import System.Posix.Process ( exitImmediately )
import System.Posix.Signals ( Handler(..), installHandler, sigQUIT )

-- | Worker process settings
--
-- Since 0.1.0.0
--
data GracefulWorker = forall resource .
    GracefulWorker { gracefulWorkerInitialize :: IO resource
                   , gracefulWorkerApplication :: Socket -> resource -> IO ()
                   , gracefulWorkerFinalize :: resource -> IO ()
                   }

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | Worker process action
workerProcess :: GracefulWorker -> Socket -> IO ()
workerProcess GracefulWorker { gracefulWorkerInitialize = initialize
                             , gracefulWorkerApplication = application
                             , gracefulWorkerFinalize = finalize
                             } sock = do
  void $ installHandler sigQUIT (CatchOnce $ close sock) Nothing
  count <- newTVarIO (0 :: Int)
  void $ tryIO $ bracket initialize finalize $ \resource ->
      void $ forever $ do
        (s, _) <- accept sock
        let app = application s resource
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
