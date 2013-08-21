-- |
-- Module      : System.Posix.Graceful.Handler
-- Copyright   : 2013 Noriyuki OHKAWA
-- License     : BSD3
--
-- Maintainer  : n.ohkawa@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- Signal handlers
module System.Posix.Graceful.Handler
    ( HandlerSettings(..)
    , resetHandlers
    , defaultHandlers
    ) where

import Control.Concurrent.STM ( atomically, TVar, newTVarIO, readTVar, modifyTVar' )
import Control.Monad ( void, unless )
import System.Exit ( ExitCode(..) )
import System.Posix.Process ( getAnyProcessStatus, exitImmediately )
import System.Posix.Signals ( Signal, signalProcess
                            , Handler(..), installHandler, fullSignalSet
                            , sigQUIT, sigHUP, sigINT, sigTERM, sigUSR2 )
import System.Posix.Types ( ProcessID )

-- | Signal handler settings
data HandlerSettings =
    HandlerSettings { handlerSettingsProcessIDs :: TVar [ProcessID]
                    , handlerSettingsQuitProcess :: IO ()
                    , handlerSettingsLaunchWorkers :: IO [ProcessID]
                    , handlerSettingsSpawnProcess :: IO ()
                    }

-- | Reset handlers by settings
resetHandlers :: HandlerSettings -> IO ()
resetHandlers settings = do
  void $ installHandler sigQUIT (CatchOnce $ handleSIGQUIT settings) (Just fullSignalSet)
  void $ installHandler sigHUP  (CatchOnce $ handleSIGHUP  settings) (Just fullSignalSet)
  void $ installHandler sigINT  (CatchOnce $ handleSIGINT  settings) (Just fullSignalSet)
  void $ installHandler sigTERM (CatchOnce $ handleSIGTERM settings) (Just fullSignalSet)
  void $ installHandler sigUSR2 (CatchOnce $ handleSIGUSR2 settings) (Just fullSignalSet)

-- | Set default handlers
defaultHandlers :: IO ()
defaultHandlers = do
  void $ installHandler sigQUIT Default Nothing
  void $ installHandler sigHUP  Default Nothing
  void $ installHandler sigINT  Default Nothing
  void $ installHandler sigTERM Default Nothing
  void $ installHandler sigUSR2 Default Nothing

broadcastSignal :: HandlerSettings -> Signal -> IO ()
broadcastSignal settings s = do
  pids <- atomically $ readTVar $ handlerSettingsProcessIDs settings
  mapM_ (signalProcess s) pids

waitAllProcess :: HandlerSettings -> IO ()
waitAllProcess settings = do
  status <- getAnyProcessStatus True False
  case status of
    Nothing -> return ()
    Just (pid, _) -> do
                remain <- atomically $ do
                            modifyTVar' (handlerSettingsProcessIDs settings) (filter (pid /=))
                            readTVar (handlerSettingsProcessIDs settings)
                unless (null remain) $ waitAllProcess settings

shutdownGracefully :: HandlerSettings -> IO ()
shutdownGracefully settings = do
  broadcastSignal settings sigQUIT
  waitAllProcess settings

-- fast shutdown
handleSIGINT :: HandlerSettings -> IO ()
handleSIGINT settings = do
  broadcastSignal settings sigINT
  waitAllProcess settings
  exitImmediately $ ExitFailure 130 -- SIGINT exit code

-- fast shutdown
handleSIGTERM :: HandlerSettings -> IO ()
handleSIGTERM settings = do
  broadcastSignal settings sigTERM
  waitAllProcess settings
  exitImmediately $ ExitFailure 143 -- SIGTERM exit code

-- graceful shutdown
handleSIGQUIT :: HandlerSettings -> IO ()
handleSIGQUIT settings = do
  shutdownGracefully settings
  handlerSettingsQuitProcess settings

-- starting new worker processes, graceful shutdown of old worker processes
handleSIGHUP :: HandlerSettings -> IO ()
handleSIGHUP settings = do
  newpids <- handlerSettingsLaunchWorkers settings >>= newTVarIO
  resetHandlers settings { handlerSettingsProcessIDs = newpids }
  shutdownGracefully settings

handleSIGUSR2 :: HandlerSettings -> IO ()
handleSIGUSR2 settings = do
  handlerSettingsSpawnProcess settings
  resetHandlers settings
