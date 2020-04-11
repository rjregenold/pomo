module Pomo.Component.HTML.Utils where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH

maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f =
  if cond
  then f unit
  else HH.text ""
