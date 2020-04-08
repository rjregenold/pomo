module Pomo.Data.Time where
  
import Prelude

import Data.Codec.Argonaut as CA
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Maybe (Maybe(..))
import Data.Newtype (over2, unwrap)
import Data.Time.Duration (class Duration, Milliseconds(..), fromDuration, toDuration)

durationCodec :: forall a. Duration a => CA.JsonCodec a
durationCodec = CA.prismaticCodec (Just <<< toDuration <<< Milliseconds) (unwrap <<< fromDuration) CA.number

instantCodec :: CA.JsonCodec Instant
instantCodec = CA.prismaticCodec (instant <<< Milliseconds) (unwrap <<< unInstant) CA.number

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
