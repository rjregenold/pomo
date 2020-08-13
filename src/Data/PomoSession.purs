module Pomo.Data.PomoSession where

import Prelude

import Control.Bind (bindFlipped)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Generic as CAG
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.Enum (fromEnum, succ)
import Data.Formatter.DateTime as Format
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List as List
import Data.Maybe (Maybe, fromMaybe)
import Data.Time.Duration (Minutes(..))
import Pomo.Capability.LocalStorage (class LocalStorage, getItem, setItem)
import Pomo.Capability.Now (class Now, now, nowDateTimeLocal)
import Pomo.Data.Argonaut as A
import Pomo.Data.PomoCount (PomoCount, pomoCountCodec)
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)

data TimerType
  = Pomodoro
  | ShortBreak
  | LongBreak

derive instance eqTimerType :: Eq TimerType
derive instance genericTimerType :: Generic TimerType _
instance showTimerType :: Show TimerType where
  show = genericShow

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

tickSessionM :: forall m. Now m => TimerSettings -> PomoSession -> m PomoSession
tickSessionM timerSettings sess = do
  currentTime <- now
  pure $ tickSession timerSettings currentTime sess

tickSession :: TimerSettings -> Instant -> PomoSession -> PomoSession
tickSession timerSettings currentTime sess =
  case sess.currentTimer.timer of
    Timer.NotRunning _ -> sess
    Timer.Running t -> do
      if isComplete
      then nextSession nextPomoCount
      else sess { currentTimer = sess.currentTimer { timer = timer } }
  where
  timer = Timer.tick sess.currentTimer.timer currentTime
  isComplete = Timer.isComplete timer
  nextPomoCount = 
    if isComplete && sess.currentTimer.timerType == Pomodoro
    then fromMaybe top $ succ sess.completedPomos
    else sess.completedPomos
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

applyUpdatedSettings :: PomoSession -> TimerSettings -> PomoSession
applyUpdatedSettings sess timerSettings = 
  let d = case sess.currentTimer.timerType of
            Pomodoro -> timerSettings.pomoDuration
            LongBreak -> timerSettings.longBreakDuration
            ShortBreak -> timerSettings.shortBreakDuration
   in sess { currentTimer = sess.currentTimer { timer = Timer.updateDuration sess.currentTimer.timer d } }

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

isTimerRunning :: PomoSession -> Boolean
isTimerRunning sess = Timer.isRunning sess.currentTimer.timer

sessionKey :: DateTime -> String
sessionKey dateTime = "pomoSession-" <> Format.format format dateTime
  where
  format = List.fromFoldable
    [ Format.YearFull
    , Format.MonthTwoDigits
    , Format.DayOfMonthTwoDigits
    ]

saveSession 
  :: forall m
   . LocalStorage m 
  => Now m
  => PomoSession 
  -> m Unit
saveSession sess = do
  local <- nowDateTimeLocal 
  setItem (sessionKey local) (A.encode pomoSessionCodec sess)

restoreSession 
  :: forall m
   . LocalStorage m 
  => Now m
  => m (Maybe PomoSession)
restoreSession = do
  local <- nowDateTimeLocal
  pure <<< bindFlipped (A.decode pomoSessionCodec) =<< getItem (sessionKey local)
