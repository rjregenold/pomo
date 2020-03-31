module Pomo.Data.Time where
  
import Prelude

import Control.Biapply (biapply)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Newtype (over2)
import Data.Time.Duration (Milliseconds(..))

-- | gets the milliseconds between the first and second Instant
instantDiff :: Instant -> Instant -> Milliseconds
instantDiff a b = over2 Milliseconds (-) (unInstant a) (unInstant b)
