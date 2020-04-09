module Pomo.Data.PomoCount 
  ( PomoCount
  , pomoCountCodec
  ) where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype PomoCount = PomoCount Int

derive instance newtypePomoCount :: Newtype PomoCount _
derive instance genericPomoCount :: Generic PomoCount _
derive newtype instance eqPomoCount :: Eq PomoCount
derive newtype instance ordPomoCount :: Ord PomoCount

instance boundedPomoCount :: Bounded PomoCount where
  bottom = PomoCount 0
  top = PomoCount 48

instance enumPomoCount :: Enum PomoCount where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumPomoCount :: BoundedEnum PomoCount where
  cardinality = Cardinality 49
  toEnum n
    | n >= 0 && n <= 48 = Just (PomoCount n)
    | otherwise = Nothing
  fromEnum (PomoCount n) = n

instance showPomoCount :: Show PomoCount where
  show = genericShow

pomoCountCodec :: CA.JsonCodec PomoCount
pomoCountCodec = CA.prismaticCodec toEnum fromEnum CA.int
