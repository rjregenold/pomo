module Pomo.Data.Time where
  
import Prelude

import Data.DateTime.Instant (Instant, unInstant)
import Data.Int (floor, round, toNumber)
import Data.Maybe (Maybe)
import Data.Newtype (over, over2, unwrap)
import Data.Time.Duration (class Duration, Milliseconds(..), Minutes(..), Seconds(..), convertDuration, fromDuration)
import Data.Tuple

-- | gets the milliseconds between the first and second Instant
instantDiff :: Instant -> Instant -> Milliseconds
instantDiff a b = over2 Milliseconds (-) (unInstant a) (unInstant b)

-- | returns true if the duration is greater than zero milliseconds
isPosDuration :: forall a. Duration a => a -> Boolean
isPosDuration d = 
  let (Milliseconds m) = fromDuration d
   in m > 0.0

-- | returns true if the duration is less than or equal to zero milliseconds
isNegDuration :: forall a. Duration a => a -> Boolean
isNegDuration = not <<< isPosDuration
