module Pomo.Capability.LocalStorage where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Maybe(Maybe)
import Halogen (HalogenM)

class Monad m <= LocalStorage m where
  getItem :: String -> m (Maybe String)
  setItem :: String -> String -> m Unit
  removeItem :: String -> m Unit
  clear :: m Unit

instance localStorageHalogenM :: LocalStorage m => LocalStorage (HalogenM st act slots msg m) where
  getItem = lift <<< getItem
  setItem key = lift <<< setItem key
  removeItem = lift <<< removeItem
  clear = lift clear
