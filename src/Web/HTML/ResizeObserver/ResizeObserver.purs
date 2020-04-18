module Pomo.Web.HTML.ResizeObserver.ResizeObserver where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Web.HTML.HTMLElement (DOMRect, HTMLElement)

foreign import data ResizeObserver :: Type
foreign import data ResizeObserverEntry :: Type

foreign import _create :: (Array ResizeObserverEntry -> ResizeObserver -> Effect Unit) -> Effect ResizeObserver

create :: (Array ResizeObserverEntry -> ResizeObserver -> Effect Unit) -> Effect ResizeObserver
create callback = _create callback

foreign import _observe :: Fn2 ResizeObserver HTMLElement (Effect Unit)

observe :: ResizeObserver -> HTMLElement -> Effect Unit
observe ob = runFn2 _observe ob

foreign import _disconnect :: ResizeObserver -> Effect Unit

disconnect :: ResizeObserver -> Effect Unit
disconnect = _disconnect

foreign import _unobserve :: Fn2 ResizeObserver HTMLElement (Effect Unit)

unobserve :: ResizeObserver -> HTMLElement -> Effect Unit
unobserve ob = runFn2 _unobserve ob

type BoxSize = 
  { blockSize :: Number
  , inlineSize :: Number
  }

foreign import _borderBoxSize :: ResizeObserverEntry -> BoxSize

borderBoxSize :: ResizeObserverEntry -> BoxSize
borderBoxSize = _borderBoxSize

foreign import _contentBoxSize :: ResizeObserverEntry -> BoxSize

contentBoxSize :: ResizeObserverEntry -> BoxSize
contentBoxSize = _contentBoxSize

foreign import _contentRect :: ResizeObserverEntry -> DOMRect

contentRect :: ResizeObserverEntry -> DOMRect
contentRect = _contentRect

foreign import _target :: ResizeObserverEntry -> HTMLElement

target :: ResizeObserverEntry -> HTMLElement
target = _target
