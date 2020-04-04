module Pomo.Data.Timer where

import Prelude

import Data.Array (replicate)
import Data.DateTime.Instant (Instant)
import Data.Int (floor, toNumber)
import Data.Newtype (over, unwrap)
import Data.Number.Format (fixed, toStringWith)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (class Duration, Milliseconds(..), Minutes(..), Seconds(..), convertDuration, fromDuration, negateDuration)
import Pomo.Capability.Now (class Now, now)
import Pomo.Data.Time (instantDiff, isNegDuration)

type RunningTimerState =
  { duration :: Minutes
  , currentTime :: Instant
  , startedAt :: Instant
  }

data Timer
  = NotRunning Minutes
  | Running RunningTimerState

derive instance eqTimer :: Eq Timer

-- | returns the number of milliseconds remaining for the given timer
remainingMs :: Timer -> Milliseconds
remainingMs = case _ of
  NotRunning d -> fromDuration d
  Running t ->
    let diffMs = instantDiff t.currentTime t.startedAt
     in fromDuration t.duration <> (negateDuration diffMs)

-- | returns true if the timer is complete
isComplete :: Timer -> Boolean
isComplete = isNegDuration <<< remainingMs

-- | takes a timer and moves it along to the next tick
tickM :: forall m. Now m => Timer -> m Timer
tickM timer = do
  currentTime <- now
  pure (tick timer currentTime)

tick :: Timer -> Instant -> Timer
tick timer currentTime = case timer of
  NotRunning _ -> timer
  Running t -> Running t { currentTime = currentTime }

startTimer :: Timer -> Instant -> Timer
startTimer timer currentTime = case timer of
  NotRunning d -> Running
    { currentTime: currentTime
    , startedAt: currentTime
    , duration: d
    }
  Running _ -> timer

stopTimer :: Timer -> Minutes -> Timer
stopTimer timer d = case timer of
  NotRunning _ -> timer
  Running _ -> NotRunning d

padStart :: Int -> Char -> String -> String
padStart n c v =
  let prefix = fromCharArray (replicate (n - length v) c)
   in prefix <> v

renderDurationAsMinSec :: forall a. Duration a => a -> String
renderDurationAsMinSec d =
  let ms = fromDuration d
      remainingMinutes = convertDuration ms :: Minutes
      roundedMinutes = over Minutes (toNumber <<< floor) remainingMinutes
      remainingSeconds = convertDuration (remainingMinutes <> negateDuration roundedMinutes) :: Seconds
      roundedSeconds = over Seconds (toNumber <<< floor) remainingSeconds :: Seconds
      f = toStringWith (fixed 0)
      mr = padStart 2 '0' (f (unwrap roundedMinutes))
      sr = padStart 2 '0' (f (unwrap roundedSeconds))
   in mr <> ":" <> sr

render :: Timer -> String
render timer = 
  let ms = remainingMs timer
      ms' = max (Milliseconds 0.0) ms
   in renderDurationAsMinSec ms'
