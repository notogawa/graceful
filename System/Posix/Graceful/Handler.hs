module System.Posix.Graceful.Handler
    ( HandlerSettings(..)
    , resetHandlers
    , defaultHandlers
    ) where

import Control.Monad ( void )
import System.Exit ( ExitCode(..) )
import System.Posix.Process ( getProcessStatus, exitImmediately )
import System.Posix.Signals ( Signal, signalProcess
                            , Handler(..), installHandler
                            , keyboardTermination, lostConnection
                            , keyboardSignal, softwareTermination
                            , userDefinedSignal2 )
import System.Posix.Types ( ProcessID )

data HandlerSettings =
    HandlerSettings { handlerSettingsProcessIDs :: [ProcessID]
                    , handlerSettingsQuitProcess :: IO ()
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

broadcastSignal :: Signal -> [ProcessID] -> IO ()
broadcastSignal = mapM_ . signalProcess

waitAllProcess :: [ProcessID] -> IO ()
waitAllProcess = mapM_ $ getProcessStatus True True

shutdownGracefully :: [ProcessID] -> IO ()
shutdownGracefully pids = do
  broadcastSignal keyboardTermination pids
  waitAllProcess pids

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
