module Pomo.Component.Modal where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Hooks as Hooks
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Pomo.Component.HTML.Utils (maybeElem)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

initializeWith 
  :: forall state action slots output m
   . MonadAff m
  => (KE.KeyboardEvent -> Maybe action)
  -> H.HalogenM state action slots output m H.SubscriptionId
initializeWith toAction = do
  document <- H.liftEffect (document =<< window)
  H.subscribe
    $ ES.eventListenerEventSource
      KET.keydown
      (HTMLDocument.toEventTarget document)
      (toAction <=< KE.fromEvent)

hooksInitWith
  :: forall action m
   . MonadAff m
  => (KE.KeyboardEvent -> Maybe (Hooks.HookM m Unit))
  -> Hooks.HookM m H.SubscriptionId
hooksInitWith toAction = do
  doc <- H.liftEffect $ document =<< window
  Hooks.subscribe do
    ES.eventListenerEventSource
      KET.keydown
      (HTMLDocument.toEventTarget doc)
      (\e -> toAction =<< KE.fromEvent e)

whenClose
  :: forall state action slots output m
   . MonadAff m
  => KE.KeyboardEvent
  -> H.SubscriptionId
  -> H.HalogenM state action slots output m Unit
  -> H.HalogenM state action slots output m Unit
whenClose ev sid close =
  when (KE.code ev == "Escape") do
    H.unsubscribe sid
    close

hooksWhenClose
  :: forall action m
   . MonadAff m
  => KE.KeyboardEvent
  -> H.SubscriptionId
  -> Hooks.HookM m Unit
  -> Hooks.HookM m Unit
hooksWhenClose ev sid close =
  when (KE.code ev == "Escape") do
    Hooks.unsubscribe sid
    close

modal 
  :: forall action slots m
   . Maybe action
  -> Array (H.ComponentHTML action slots m)
  -> H.ComponentHTML action slots m
modal click html =
  HH.div
    [ HP.classes modalClasses ]
    [ HH.a
      [ HP.classes overlayClasses
      , HPA.label "Close"
      , HE.onClick \_ -> click
      ]
      []
    , HH.div
      [ HP.classes containerClasses ]
      html
    ]

type HeaderProps i =
  { title :: Maybe String
  , action :: Maybe i
  }

header 
  :: forall p i
   . HeaderProps i
  -> HH.HTML p i
header props = 
  HH.div
    [ HP.classes headerClasses ]
    [ buttonHeaderClose props.action
    , maybeElem props.title $ \title ->
      HH.div
        [ HP.classes modalTitleClasses
        ]
        [ HH.text title
        ]
    ]

buttonHeaderClose :: forall p i. Maybe i -> HH.HTML p i
buttonHeaderClose action =
  HH.a
    [ HP.classes buttonHeaderCloseClasses
    , HPA.label "Close"
    , HE.onClick \_ -> action
    ]
    []

body 
  :: forall p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
body html =
  HH.div
    [ HP.classes bodyClasses ]
    [ content html ]

content
  :: forall p i
   . Array (HH.HTML p i)
  -> HH.HTML p i
content html =
  HH.div
    [ HP.classes contentClasses ]
    html

type FooterProps p i =
  { buttons :: Array (HH.HTML p i)
  }

footer 
  :: forall p i
   . FooterProps p i
  -> HH.HTML p i
footer props =
  HH.div
    [ HP.classes footerClasses ]
    props.buttons

modalClasses :: Array HH.ClassName
modalClasses = HH.ClassName <$>
  [ "modal"
  , "active"
  ]

overlayClasses :: Array HH.ClassName
overlayClasses = HH.ClassName <$>
  [ "modal-overlay"
  ]

containerClasses :: Array HH.ClassName
containerClasses = HH.ClassName <$>
  [ "modal-container"
  ]

headerClasses :: Array HH.ClassName
headerClasses = HH.ClassName <$>
  [ "modal-header"
  ]

buttonHeaderCloseClasses :: Array HH.ClassName
buttonHeaderCloseClasses = HH.ClassName <$>
  [ "btn"
  , "btn-clear"
  , "float-right"
  ]

modalTitleClasses :: Array HH.ClassName
modalTitleClasses = HH.ClassName <$>
  [ "modal-title"
  , "h5"
  ]

bodyClasses :: Array HH.ClassName
bodyClasses = HH.ClassName <$>
  [ "modal-body"
  ]

contentClasses :: Array HH.ClassName
contentClasses = HH.ClassName <$>
  [ "content"
  ]

footerClasses :: Array HH.ClassName
footerClasses = HH.ClassName <$>
  [ "modal-footer"
  ]
