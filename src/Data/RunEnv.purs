-- | The runtime environment.
module Pomo.Data.RunEnv where

import Prelude

data RunEnv
  = Dev
  | Prod

derive instance eqRunEnv :: Eq RunEnv
derive instance ordRunEnv :: Ord RunEnv
