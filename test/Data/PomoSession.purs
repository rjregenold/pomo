module Test.Pomo.Data.PomoSession where

import Prelude

import Data.DateTime.Instant as Instant
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time.Duration as Duration
import Effect (Effect)
import Effect.Console (log)
import Pomo.Data.Argonaut as A
import Pomo.Data.PomoSession as PomoSession
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings as TimerSettings
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)

main :: Effect Unit
main = do
  let settings = unsafePartial $ fromJust TimerSettings.defaultTimerSettings
      startMs = Duration.Milliseconds 1577836800000.0
      endMs = startMs <> Duration.convertDuration settings.pomoDuration
      startInstant = unsafePartial $ fromJust $ Instant.instant startMs
      doneInstant = unsafePartial $ fromJust $ Instant.instant endMs

  log "test initPomoSession"
  let sess = PomoSession.initPomoSession settings.pomoDuration
  assert (sess.currentTimer.timerType == PomoSession.Pomodoro)
  assert (sess.currentTimer.timer == Timer.NotRunning settings.pomoDuration)

  log "test nextTimer"
  assert (PomoSession.nextTimer bottom PomoSession.ShortBreak settings == { timer: Timer.NotRunning settings.pomoDuration, timerType: PomoSession.Pomodoro  })
  assert (PomoSession.nextTimer bottom PomoSession.LongBreak settings == { timer: Timer.NotRunning settings.pomoDuration, timerType: PomoSession.Pomodoro  })
  assert (PomoSession.nextTimer bottom PomoSession.Pomodoro settings == { timer: Timer.NotRunning settings.shortBreakDuration, timerType: PomoSession.ShortBreak })
  assert (PomoSession.nextTimer settings.pomosBetweenLongBreak PomoSession.Pomodoro settings == { timer: Timer.NotRunning settings.longBreakDuration, timerType: PomoSession.LongBreak })

  log "test tickSession"
  let runningSess = PomoSession.startTimer sess startInstant
  assert (PomoSession.tickSession settings startInstant sess == sess)
  let doneSess = { currentTimer: { timer: Timer.NotRunning settings.shortBreakDuration, timerType: PomoSession.ShortBreak }, completedPomos: _ } <$> toEnum 1
  assert (PomoSession.tickSession settings doneInstant runningSess == (unsafePartial $ fromJust doneSess))

  log "test codec"
  assert (roundTrip PomoSession.pomoSessionCodec sess == Just sess)

  where

  roundTrip codec = A.decode codec <<< A.encode codec
