module Pomo.Data.PomoSession where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Enum (fromEnum, succ)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Minutes(..))
import Pomo.Capability.Now (class Now, now)
import Pomo.Data.PomoCount (PomoCount)
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)

type PomoSession =
  { currentTimer :: PomoTimer
  , completedPomos :: PomoCount
  }

data TimerType
  = Pomodoro
  | ShortBreak
  | LongBreak

derive instance eqTimerType :: Eq TimerType

type PomoTimer =
  { timer :: Timer.Timer
  , timerType :: TimerType
  }

defaultPomoSession :: PomoSession
defaultPomoSession = initPomoSession $ Minutes 25.0

initPomoSession :: Minutes -> PomoSession
initPomoSession d =
  { currentTimer:
    { timer: Timer.NotRunning d
    , timerType: Pomodoro
    }
  , completedPomos: bottom
  }

tickSessionM :: forall m. Now m => PomoSession -> TimerSettings -> m (Maybe PomoSession)
tickSessionM sess timerSettings = do
  currentTime <- now
  pure $ tickSession sess timerSettings currentTime

tickSession :: PomoSession -> TimerSettings -> Instant -> Maybe PomoSession
tickSession sess timerSettings currentTime =
  case sess.currentTimer.timer of
    Timer.NotRunning _ -> Just sess
    Timer.Running t -> do
      if Timer.isComplete timer
      then map nextSession (succ sess.completedPomos)
      else Just sess { currentTimer = sess.currentTimer { timer = timer } }
  where
  timer = Timer.tick sess.currentTimer.timer currentTime
  nextSession n = sess
    { currentTimer = nextTimer n sess.currentTimer.timerType timerSettings
    , completedPomos = n
    }

nextTimer :: PomoCount -> TimerType -> TimerSettings -> PomoTimer
nextTimer completedPomos completedTimerType timerSettings =
  case completedTimerType of
    Pomodoro -> 
      let n = fromEnum completedPomos
          p = fromEnum timerSettings.pomosBetweenLongBreak
       in if n > 0 && n `mod` p == 0
          then { timer: Timer.NotRunning timerSettings.longBreakDuration
               , timerType: LongBreak
               }
          else { timer: Timer.NotRunning timerSettings.shortBreakDuration
               , timerType: ShortBreak
               }
    _ ->
      { timer: Timer.NotRunning timerSettings.pomoDuration
      , timerType: Pomodoro
      }

startTimer :: PomoSession -> Instant -> PomoSession
startTimer sess currentTime = sess
  { currentTimer = sess.currentTimer
    { timer = Timer.startTimer sess.currentTimer.timer currentTime 
    }
  }

stopTimer :: PomoSession -> TimerSettings -> PomoSession
stopTimer sess timerSettings = sess
  { currentTimer = sess.currentTimer
    { timer = Timer.NotRunning timerSettings.pomoDuration
    , timerType = Pomodoro
    }
  }
