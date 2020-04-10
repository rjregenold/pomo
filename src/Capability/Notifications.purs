module Pomo.Capability.Notifications where

import Prelude

class Monad m <= Notifications m where
  requestPermission :: m Unit
