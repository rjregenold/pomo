module Pomo.Data.PomoSession where

import Prelude

import Data.Argonaut (fromString, stringify)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Generic as CAG
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime.Instant (Instant)
import Data.Either (hush)
import Data.Enum (fromEnum, succ)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Minutes(..))
import Pomo.Capability.LocalStorage (class LocalStorage, getItem, setItem)
import Pomo.Capability.Now (class Now, now)
import Pomo.Data.PomoCount (PomoCount, pomoCountCodec)
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)

data TimerType
  = Pomodoro
  | ShortBreak
  | LongBreak

derive instance eqTimerType :: Eq TimerType
derive instance genericTimerType :: Generic TimerType _

timerTypeCodec :: CA.JsonCodec TimerType
timerTypeCodec = CAG.nullarySum "TimerType"

type PomoTimer =
  { timer :: Timer.Timer
  , timerType :: TimerType
  }

pomoTimerCodec :: CA.JsonCodec PomoTimer
pomoTimerCodec =
  CA.object "PomoTimer"
    (CAR.record
      { timer: Timer.timerCodec
      , timerType: timerTypeCodec
      })

type PomoSession =
  { currentTimer :: PomoTimer
  , completedPomos :: PomoCount
  }

pomoSessionCodec :: CA.JsonCodec PomoSession
pomoSessionCodec =
  CA.object "PomoSession"
    (CAR.record
      { currentTimer: pomoTimerCodec
      , completedPomos: pomoCountCodec
      })

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

sessionKey :: String
sessionKey = "pomoSession"

saveTimer :: forall m. LocalStorage m => PomoSession -> m Unit
saveTimer sess = setItem sessionKey (stringify $ CA.encode pomoSessionCodec sess)

restoreTimer :: forall m. LocalStorage m => m (Maybe PomoSession)
restoreTimer = do
  mVal <- getItem sessionKey
  case mVal of
    Nothing -> pure Nothing
    Just val -> pure (dec val)
  where
  dec = hush <<< CA.decode pomoSessionCodec <<< fromString
