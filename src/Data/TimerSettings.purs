module Pomo.Data.TimerSettings 
  ( TimerSettings
  , defaultTimerSettings
  , saveSettings
  , restoreSettings
  )
  where

import Prelude

import Control.Bind (bindFlipped)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Enum (toEnum)
import Data.Maybe (Maybe, fromJust)
import Data.Time.Duration (Minutes(..))
import Partial.Unsafe (unsafePartial)
import Pomo.Capability.LocalStorage (class LocalStorage, getItem, setItem)
import Pomo.Data.Argonaut as A
import Pomo.Data.PomoCount (PomoCount, pomoCountCodec)
import Pomo.Data.Time (minutesCodec)

type TimerSettings =
  { pomoDuration :: Minutes
  , shortBreakDuration :: Minutes
  , longBreakDuration :: Minutes
  , pomosBetweenLongBreak :: PomoCount
  , pomoDailyGoal:: PomoCount
  }

timerSettingsCodec :: CA.JsonCodec TimerSettings
timerSettingsCodec =
  CA.object "TimerSettings"
    (CAR.record
      { pomoDuration: minutesCodec
      , shortBreakDuration: minutesCodec
      , longBreakDuration: minutesCodec
      , pomosBetweenLongBreak: pomoCountCodec
      , pomoDailyGoal: pomoCountCodec
      })

defaultTimerSettings :: TimerSettings
-- i am ok with verifying for the compiler that the default timer
-- settings are valid. also, there is a test to ensure this does
-- not throw a runtime error.
defaultTimerSettings = unsafePartial $ fromJust $ defaults
  <$> toEnum 4
  <*> toEnum 12
  where defaults =
          { pomoDuration: Minutes 25.0
          , shortBreakDuration: Minutes 5.0
          , longBreakDuration: Minutes 15.0
          , pomosBetweenLongBreak: _
          , pomoDailyGoal: _
          }

settingsKey :: String
settingsKey = "timerSettings"

saveSettings
  :: forall m
   . LocalStorage m
  => TimerSettings
  -> m Unit
saveSettings = setItem settingsKey <<< A.encode timerSettingsCodec

restoreSettings
  :: forall m
   . LocalStorage m
  => m (Maybe TimerSettings)
restoreSettings = pure <<< bindFlipped (A.decode timerSettingsCodec) =<< getItem settingsKey
