module Timer ( Timer,
               mkTimer,
               start, stop, toggleStart,
               pause, unpause, togglePause,
               isStarted, isPaused,
               getTicks )
       where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Monad (when)

import Data.Word (Word32)

import GHC.Conc (unsafeIOToSTM)

import qualified Graphics.UI.SDL as SDL


data TimerState = Started | Stopped | Paused
                deriving Eq

data Timer =
  Timer { state       :: TVar TimerState
        , startTicks  :: TVar Word32
        , pausedTicks :: TVar Word32 }

mkTimer :: IO Timer
mkTimer = do
  Timer <$> newTVarIO Stopped
        <*> newTVarIO 0
        <*> newTVarIO 0

start :: Timer -> IO ()
start = atomically . startSTM

startSTM :: Timer -> STM ()
startSTM (Timer state startTicks _) = do
  ticks <- unsafeIOToSTM SDL.getTicks

  state' <- readTVar state
  when (state' /= Started) $ do
    writeTVar state Started
    writeTVar startTicks ticks

stop :: Timer -> IO ()
stop = atomically . stopSTM

stopSTM :: Timer -> STM ()
stopSTM (Timer state startTicks pausedTicks) = do
  state' <- readTVar state
  when (state' /= Stopped) $ do
    writeTVar state Stopped
    writeTVar startTicks 0
    writeTVar pausedTicks 0

toggleStart :: Timer -> IO ()
toggleStart timer =
  atomically $ do
    started <- isStartedSTM timer
    if started
      then stopSTM timer
      else startSTM timer

pause :: Timer -> IO ()
pause = atomically . pauseSTM

pauseSTM :: Timer -> STM ()
pauseSTM (Timer state startTicks pausedTicks) = do
  ticks <- unsafeIOToSTM $ SDL.getTicks

  state' <- readTVar state
  when (state' == Started) $ do
    start <- readTVar startTicks
    writeTVar state Paused
    writeTVar pausedTicks (ticks - start)

unpause :: Timer -> IO ()
unpause = atomically . unpauseSTM

unpauseSTM :: Timer -> STM ()
unpauseSTM (Timer state startTicks pausedTicks) = do
  ticks <- unsafeIOToSTM $ SDL.getTicks

  state' <- readTVar state
  when (state' == Paused) $ do
    paused <- readTVar pausedTicks
    writeTVar state Started
    writeTVar startTicks (ticks - paused)

togglePause :: Timer -> IO ()
togglePause timer =
  atomically $ do
    paused <- isPausedSTM timer
    if paused
      then unpauseSTM timer
      else pauseSTM timer

getTicks :: Timer -> IO Word32
getTicks (Timer state startTicks pausedTicks) = do
  atomically $ do
    ticks <- unsafeIOToSTM SDL.getTicks

    state' <- readTVar state
    case state' of
      Started -> (ticks -) <$> readTVar startTicks
      Paused  -> readTVar pausedTicks
      _       -> return 0

isPaused :: Timer -> IO Bool
isPaused = atomically . isPausedSTM

isPausedSTM :: Timer -> STM Bool
isPausedSTM (Timer state _ _) = do
  state' <- readTVar state
  case state' of
    Paused -> return True
    _      -> return False

isStarted :: Timer -> IO Bool
isStarted = atomically . isStartedSTM

isStartedSTM :: Timer -> STM Bool
isStartedSTM (Timer state _ _) = do
  state' <- readTVar state
  case state' of
    Started -> return True
    _       -> return False
