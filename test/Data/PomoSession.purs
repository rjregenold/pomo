module Test.Pomo.Data.PomoSession where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
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

  log "test initPomoSession"
  let sess = PomoSession.initPomoSession settings.pomoDuration
  assert (sess.currentTimer.timerType == PomoSession.Pomodoro)
  assert (sess.currentTimer.timer == Timer.NotRunning settings.pomoDuration)

  log "test nextTimer"
  assert (PomoSession.nextTimer bottom PomoSession.ShortBreak settings == { timer: Timer.NotRunning settings.pomoDuration, timerType: PomoSession.Pomodoro  })
  assert (PomoSession.nextTimer bottom PomoSession.LongBreak settings == { timer: Timer.NotRunning settings.pomoDuration, timerType: PomoSession.Pomodoro  })
  assert (PomoSession.nextTimer bottom PomoSession.Pomodoro settings == { timer: Timer.NotRunning settings.shortBreakDuration, timerType: PomoSession.ShortBreak })
  assert (PomoSession.nextTimer settings.pomosBetweenLongBreak PomoSession.Pomodoro settings == { timer: Timer.NotRunning settings.longBreakDuration, timerType: PomoSession.LongBreak })

  log "test codec"
  assert (roundTrip PomoSession.pomoSessionCodec sess == Just sess)

  where

  roundTrip codec = A.decode codec <<< A.encode codec
