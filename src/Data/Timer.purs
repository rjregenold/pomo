module Pomo.Data.Timer where

import Prelude

import Data.Array (replicate)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (floor, toNumber)
import Data.Newtype (over, unwrap)
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor (dimap)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray)
import Data.Symbol (SProxy(..))
import Data.Variant as V
import Data.Time.Duration (class Duration, Milliseconds(..), Minutes(..), Seconds(..), convertDuration, fromDuration, negateDuration)
import Pomo.Capability.Now (class Now, now)
import Pomo.Data.Time (durationCodec, instantCodec, instantDiff, isNegDuration, minutesCodec)

type RunningTimerState =
  { duration :: Minutes
  , currentTime :: Instant
  , startedAt :: Instant
  }

data Timer
  = NotRunning Minutes
  | Running RunningTimerState

derive instance eqTimer :: Eq Timer
derive instance genericTimer :: Generic Timer _

instance showTimer :: Show Timer where
  show = genericShow

runningTimerStateCodec :: CA.JsonCodec RunningTimerState
runningTimerStateCodec =
  CA.object "RunningTimerState"
    (CAR.record
      { duration: minutesCodec
      , currentTime: instantCodec
      , startedAt: instantCodec
      })

timerCodec :: CA.JsonCodec Timer
timerCodec = 
  dimap toVariant fromVariant $ CAV.variantMatch
    { notRunning: Right minutesCodec
    , running: Right runningTimerStateCodec
    }
  where
    toVariant = case _ of
      NotRunning m -> V.inj (SProxy :: _ "notRunning") m
      Running ts -> V.inj (SProxy :: _ "running") ts
    fromVariant = V.match
      { notRunning: NotRunning
      , running: Running
      }

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

isRunning :: Timer -> Boolean
isRunning = case _ of
  NotRunning _ -> false
  Running _ -> true

timerDuration :: Timer -> Minutes
timerDuration = case _ of
  NotRunning d -> d
  Running ts -> ts.duration

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
