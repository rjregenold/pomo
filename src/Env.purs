module Pomo.Env where

import Pomo.Data.RunEnv (RunEnv)
import Pomo.Data.TimerSettings (TimerSettings)

type Env =
  { runEnv :: RunEnv
  , timerSettings :: TimerSettings
  }
