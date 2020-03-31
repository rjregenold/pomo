module Test.Main where

import Prelude

import Effect (Effect)
import Test.Pomo.Data.TimerSettings as TimerSettings

main :: Effect Unit
main = do
  TimerSettings.main
