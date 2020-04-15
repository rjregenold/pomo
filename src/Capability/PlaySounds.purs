-- | A capability representing the ability to play a short sound file.
module Pomo.Capability.PlaySounds where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

class Monad m <= PlaySounds m where
  playSound :: String -> m Unit

instance playSoundsHalogenM :: PlaySounds m => PlaySounds (HalogenM st act slots msg m) where
  playSound = lift <<< playSound
