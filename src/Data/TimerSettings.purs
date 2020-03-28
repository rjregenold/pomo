module Pomo.Data.TimerSettings 
  ( DailyGoal
  , mkDailyGoal
  , PomosBetweenLongBreak
  , mkPomosBetweenLongBreak
  , TimerSettings
  , defaultTimerSettings
  )
  where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Minutes(..))

newtype PomosBetweenLongBreak = PomosBetweenLongBreak Int

mkPomosBetweenLongBreak :: Int -> Maybe PomosBetweenLongBreak
mkPomosBetweenLongBreak x =
  if x >= 0 && x <= 48
    then Just (PomosBetweenLongBreak x)
    else Nothing

newtype DailyGoal = DailyGoal Int

mkDailyGoal :: Int -> Maybe DailyGoal
mkDailyGoal x = 
  if x >= 1 && x <= 48
    then Just (DailyGoal x)
    else Nothing

type TimerSettings =
  { pomoDuration :: Minutes
  , shortBreakDuration :: Minutes
  , longBreakDuration :: Minutes
  , pomosBetweenLongBreak :: PomosBetweenLongBreak
  , pomoDailyGoal:: DailyGoal
  }

mkMinutes :: Int -> Minutes
mkMinutes = Minutes <<< toNumber

defaultTimerSettings :: Maybe TimerSettings
defaultTimerSettings = defaults
  <$> mkPomosBetweenLongBreak 4
  <*> mkDailyGoal 12
  where defaults =
          { pomoDuration: mkMinutes 25
          , shortBreakDuration: mkMinutes 5
          , longBreakDuration: mkMinutes 15
          , pomosBetweenLongBreak: _
          , pomoDailyGoal: _
          }
