module Pomo.Component.Hooks.UseResizeObserver
  ( useResizeObserver
  , UseResizeObserver
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Hooks (Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.HookM (StateToken)
import Halogen.Query.EventSource as ES
import Pomo.Web.HTML.ResizeObserver.ResizeObserver as RO
import Web.HTML.HTMLElement (DOMRect, HTMLElement)

newtype UseResizeObserver hooks = UseResizeObserver (UseEffect (UseState (Maybe DOMRect) hooks))

derive instance newtypeUseResizeObserver :: Newtype (UseResizeObserver hooks) _

useResizeObserver 
  :: forall slots output m
   . MonadAff m 
  => HTMLElement
  -> Hook slots output m UseResizeObserver (Maybe DOMRect)
useResizeObserver el = Hooks.wrap Hooks.do
  contentRect /\ contentRectState <- Hooks.useState Nothing

  Hooks.useLifecycleEffect do
    subscription <- observeElement contentRectState
    pure $ Just $ Hooks.unsubscribe subscription

  Hooks.pure contentRect

  where

  observeElement :: StateToken (Maybe DOMRect) -> HookM slots output m H.SubscriptionId
  observeElement contentRectState = Hooks.subscribe do
    ES.effectEventSource \emitter -> do
      ro <- RO.create \entries _ -> do
        ES.emit emitter $ Hooks.put contentRectState $ map RO.contentRect $ Array.last entries
      RO.observe ro el
      pure (ES.Finalizer (RO.disconnect ro))
