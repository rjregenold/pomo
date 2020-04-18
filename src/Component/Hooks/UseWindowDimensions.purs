module Pomo.Component.Hooks.UseWindowDimensions
  ( useWindowDimensions
  , UseWindowDimensions
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Hooks (Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.HookM (StateToken)
import Halogen.Query.EventSource as ES
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.Event.EventTarget (EventTarget)
import Web.HTML as HTML
import Web.HTML.Window as Window

type Dimensions = 
  { width :: Int
  , height :: Int 
  }

newtype UseWindowDimensions hooks = UseWindowDimensions (UseEffect (UseState (Maybe Dimensions) hooks))

derive instance newtypeUseWindowDimensions :: Newtype (UseWindowDimensions hooks) _

useWindowDimensions :: forall slots output m. MonadAff m => Hook slots output m UseWindowDimensions (Maybe Dimensions)
useWindowDimensions = Hooks.wrap Hooks.do
  dimensions /\ dimensionsState <- Hooks.useState Nothing

  Hooks.useLifecycleEffect do
    subscription <- subscribeToWindow dimensionsState
    pure $ Just $ Hooks.unsubscribe subscription

  Hooks.pure dimensions

  where

  subscribeToWindow :: StateToken (Maybe Dimensions) -> HookM slots output m H.SubscriptionId
  subscribeToWindow dimensionsState = do
    let readDimensions win = do
          w <- liftEffect $ Window.innerWidth win
          h <- liftEffect $ Window.innerHeight win
          Hooks.put dimensionsState $ Just { width: w, height: h }

    window <- liftEffect HTML.window
    subscriptionId <- Hooks.subscribe do
      ES.eventListenerEventSource
        (EventType "resize")
        (Window.toEventTarget window)
        (Event.target >>> map (fromEventTarget >>> readDimensions))

    readDimensions window
    pure subscriptionId

-- This function is missing from the purescript-web-html repository
fromEventTarget :: EventTarget -> HTML.Window
fromEventTarget = unsafeCoerce
