module Pomo.Data.PomoSession where

import Prelude

import Data.Date (Date)
import Pomo.Data.PomoCount (PomoCount)

type PomoSession =
  { date :: Date
  , completedPomos :: PomoCount
  }
