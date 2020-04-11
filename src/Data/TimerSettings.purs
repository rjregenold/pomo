module Pomo.Data.TimerSettings 
  ( TimerSettings
  , defaultTimerSettings
  )
  where

import Prelude

import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), fromEnum, toEnum)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Minutes(..))
import Pomo.Data.PomoCount (PomoCount)

type TimerSettings =
  { pomoDuration :: Minutes
  , shortBreakDuration :: Minutes
  , longBreakDuration :: Minutes
  , pomosBetweenLongBreak :: PomoCount
  , pomoDailyGoal:: PomoCount
  }

defaultTimerSettings :: Maybe TimerSettings
defaultTimerSettings = defaults
  <$> toEnum 4
  <*> toEnum 12
  where defaults =
          { pomoDuration: Minutes 25.0
          , shortBreakDuration: Minutes 5.0
          , longBreakDuration: Minutes 15.0
          , pomosBetweenLongBreak: _
          , pomoDailyGoal: _
          }
