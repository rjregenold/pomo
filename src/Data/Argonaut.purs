module Pomo.Data.Argonaut where

import Prelude

import Control.Bind (bindFlipped)
import Data.Argonaut as J
import Data.Codec.Argonaut as CA
import Data.Either (hush)
import Data.Maybe (Maybe)

encode :: forall a. CA.JsonCodec a -> a -> String
encode codec = J.stringify <<< CA.encode codec

decode :: forall a. CA.JsonCodec a -> String -> Maybe a
decode codec = bindFlipped (hush <<< CA.decode codec) <<< hush <<< J.jsonParser
