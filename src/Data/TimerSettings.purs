module Pomo.Data.TimerSettings 
  ( DailyGoal
  , TimerSettings
  , defaultTimerSettings
  )
  where

import Prelude

import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), fromEnum, toEnum)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Time.Duration (Minutes(..))
import Pomo.Data.PomoCount (PomoCount)

newtype DailyGoal = DailyGoal Int

derive instance newtypeDailyGoal :: Newtype DailyGoal _
derive newtype instance eqDailyGoal :: Eq DailyGoal
derive newtype instance ordDailyGoal :: Ord DailyGoal

instance boundedDailyGoal :: Bounded DailyGoal where
  bottom = DailyGoal 1
  top = DailyGoal 48

instance enumDailyGoal :: Enum DailyGoal where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumDailyGoal :: BoundedEnum DailyGoal where
  cardinality = Cardinality 48
  toEnum n
    | n >= 1 && n <= 48 = Just (DailyGoal n)
    | otherwise = Nothing
  fromEnum (DailyGoal n) = n

type TimerSettings =
  { pomoDuration :: Minutes
  , shortBreakDuration :: Minutes
  , longBreakDuration :: Minutes
  , pomosBetweenLongBreak :: PomoCount
  , pomoDailyGoal:: DailyGoal
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
