module Pomo.Data.Foldable where

import Prelude

import Data.FoldableWithIndex (class FoldableWithIndex, foldrWithIndex)

foldlWithIndexM 
  :: forall a b i m t
   . Monad m
  => FoldableWithIndex i t
  => (i -> b -> a -> m b) 
  -> b 
  -> t a 
  -> m b
foldlWithIndexM f acc xs = foldrWithIndex f' pure xs acc
  where f' i x k z = f i z x >>= k
