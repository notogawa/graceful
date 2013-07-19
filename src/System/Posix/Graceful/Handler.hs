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
                            , Handler(..), installHandler
                            , keyboardTermination, lostConnection
                            , keyboardSignal, softwareTermination
                            , userDefinedSignal2 )
import System.Posix.Types ( ProcessID )

data HandlerSettings =
    HandlerSettings { handlerSettingsProcessIDs :: TVar [ProcessID]
                    , handlerSettingsQuitProcess :: IO ()
                    , handlerSettingsLaunchWorkers :: IO [ProcessID]
                    , handlerSettingsSpawnProcess :: IO ()
                    }

resetHandlers :: HandlerSettings -> IO ()
resetHandlers settings = do
  void $ installHandler keyboardTermination (Catch $ handleSIGQUIT settings) Nothing
  void $ installHandler lostConnection (Catch $ handleSIGHUP settings) Nothing
  void $ installHandler keyboardSignal (Catch $ handleSIGINT settings) Nothing
  void $ installHandler softwareTermination (Catch $ handleSIGTERM settings) Nothing
  void $ installHandler userDefinedSignal2 (Catch $ handleSIGUSR2 settings) Nothing

defaultHandlers :: IO ()
defaultHandlers = do
  void $ installHandler keyboardTermination Default Nothing
  void $ installHandler lostConnection Default Nothing
  void $ installHandler keyboardSignal Default Nothing
  void $ installHandler softwareTermination Default Nothing
  void $ installHandler userDefinedSignal2 Default Nothing

broadcastSignal :: HandlerSettings -> Signal -> IO ()
broadcastSignal settings s = do
    pids <- atomically $ readTVar $ handlerSettingsProcessIDs settings
    mapM_ (signalProcess s) pids

waitAllProcess :: HandlerSettings -> IO ()
waitAllProcess settings = do
  status <- getAnyProcessStatus True False
  -- appendFile "/tmp/log" $ shows status "\n"
  case status of
    Nothing -> return ()
    Just (pid, _s) -> do
                remain <-atomically $ do
                            modifyTVar' (handlerSettingsProcessIDs settings) (filter (pid /=))
                            readTVar (handlerSettingsProcessIDs settings)
                unless (null remain) $ waitAllProcess settings

shutdownGracefully :: HandlerSettings -> IO ()
shutdownGracefully settings = do
  broadcastSignal settings keyboardTermination
  waitAllProcess settings

-- fast shutdown
handleSIGINT :: HandlerSettings -> IO ()
handleSIGINT settings = do
  broadcastSignal settings keyboardSignal
  waitAllProcess settings
  exitImmediately $ ExitFailure 130 -- SIGINT exit code

-- fast shutdown
handleSIGTERM :: HandlerSettings -> IO ()
handleSIGTERM settings = do
  broadcastSignal settings softwareTermination
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
  shutdownGracefully settings
  handlerSettingsQuitProcess settings
