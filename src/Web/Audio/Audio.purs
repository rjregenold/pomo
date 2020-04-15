module Pomo.Web.Audio.Audio where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import data HTMLAudioElement :: Type

data AudioVolume
  = Silent
  | Full
  | SetVolume Volume

newtype Volume = Volume Number

toVolume :: Number -> Maybe Volume
toVolume x
  | x >= 0.0 && x <= 1.0 = Just (Volume x)
  | otherwise = Nothing

foreign import _create :: String -> Effect HTMLAudioElement

create :: String -> Effect HTMLAudioElement
create = _create

foreign import _play :: HTMLAudioElement -> Effect Unit

play :: HTMLAudioElement -> Effect Unit
play = _play

foreign import _setVolume :: Fn2 HTMLAudioElement Number (Effect Unit)

setVolume :: HTMLAudioElement -> AudioVolume -> Effect Unit
setVolume el = runFn2 _setVolume el <<< to
  where
  to = case _ of
         Silent -> 0.0
         Full -> 1.0
         SetVolume (Volume x) -> x
