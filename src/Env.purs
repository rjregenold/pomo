module Pomo.Env where

import Pomo.Data.AssetUrls (AssetUrls)
import Pomo.Data.RunEnv (RunEnv)
import Pomo.Data.TimerSettings (TimerSettings)

type WithEnv r =
  ( runEnv :: RunEnv
  , assetUrls :: AssetUrls
  , timerSettings :: TimerSettings
  | r
  )

type Env =
  { runEnv :: RunEnv
  , assetUrls :: AssetUrls
  , timerSettings :: TimerSettings
  }
