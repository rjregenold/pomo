module Pomo.Env where

import Pomo.Data.AssetUrls (AssetUrls)
import Pomo.Data.RunEnv (RunEnv)

type WithEnv r =
  ( runEnv :: RunEnv
  , assetUrls :: AssetUrls
  | r
  )

type Env =
  { runEnv :: RunEnv
  , assetUrls :: AssetUrls
  }
