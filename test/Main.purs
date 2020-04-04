module Test.Main where

import Prelude

import Effect (Effect)
import Test.Pomo.Data.Timer as Timer
import Test.Pomo.Data.TimerSettings as TimerSettings
import Test.Pomo.Data.PomoSession as PomoSession

main :: Effect Unit
main = do
  Timer.main
  TimerSettings.main
  PomoSession.main
