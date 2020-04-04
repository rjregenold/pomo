module Test.Pomo.Data.Timer where

import Prelude

import Data.Date as Date
import Data.Time as Time
import Data.DateTime as DateTime
import Data.DateTime.Instant as Instant
import Data.Enum (toEnum)
import Data.Maybe (Maybe, fromJust)
import Data.String (length)
import Data.Time.Duration as Duration
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Pomo.Data.Timer as Timer
import Test.Assert (assert)
import Test.QuickCheck (quickCheck)

main :: Effect Unit
main = do
  log "checking padStart"
  quickCheck \n -> length (Timer.padStart n 'a' mempty) >= n
  quickCheck \str -> length (Timer.padStart 10 'a' str) >= 10
  assert (Timer.padStart 2 '0' "1" == "01")
  assert (Timer.padStart 2 '0' "15" == "15")
  assert (Timer.padStart 2 '0' "" == "00")
  assert (Timer.padStart (-2) '0' mempty == mempty)

  let date = unsafePartial (fromJust (Date.canonicalDate <$> (toEnum 2020) <*> (toEnum 1) <*> (toEnum 1)))
      time = unsafePartial (fromJust (Time.Time <$> (toEnum 0) <*> (toEnum 0) <*> (toEnum 0) <*> (toEnum 0)))
      datetime = DateTime.DateTime date time
      timer = 
        { duration: Duration.Minutes 25.0
        , currentTime: Instant.fromDateTime datetime
        , startedAt: Instant.fromDateTime datetime 
        }
      timer' = timer
        { currentTime = toInstant (DateTime.adjust (Duration.Milliseconds 1032.0) datetime)
        }
      timer'' = timer
        { currentTime = toInstant (DateTime.adjust timer.duration datetime)
        }
      timer''' = timer
        { currentTime = toInstant (DateTime.adjust (timer.duration <> (Duration.Minutes 1.0)) datetime)
        }

  log "test remainingMs"
  assert (Timer.remainingMs timer == Duration.fromDuration timer.duration)
  assert (Timer.remainingMs timer'' == Duration.Milliseconds 0.0)

  log "test isComplete"
  assert (Timer.isComplete timer == false)
  assert (Timer.isComplete timer' == false)
  assert (Timer.isComplete timer'')

  log "test renderDurationAsMinSec"
  assert (Timer.renderDurationAsMinSec (Duration.Milliseconds 0.0) == "00:00")
  assert (Timer.renderDurationAsMinSec (Duration.Minutes 25.0) == "25:00")
  assert (Timer.renderDurationAsMinSec (Duration.Seconds 15.0) == "00:15")
  assert (Timer.renderDurationAsMinSec (Duration.Minutes (-1.0)) == "-1:00")

  log "test render"
  assert (Timer.render timer == "25:00")
  assert (Timer.render timer' == "24:58")
  assert (Timer.render timer'' == "00:00")
  assert (Timer.render timer''' == "00:00")

  where

  toInstant :: Maybe DateTime.DateTime -> Instant.Instant
  toInstant x = unsafePartial (fromJust (map Instant.fromDateTime x))
