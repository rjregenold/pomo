module Pomo.Component.SettingsModal where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Pomo.Capability.Navigate (class Navigate, navigate)
import Pomo.Component.HTML.Utils (whenElem)
import Pomo.Component.Modal as Modal
import Pomo.Data.Route as Route

type Slot = H.Slot Query Void

data Action
  = CloseSettings

data Query a
  = OpenSettings a

component 
  :: forall i o m
   . MonadEffect m 
  => Navigate m
  => H.Component HH.HTML Query i o m
component = Hooks.component \{ queryToken } _ -> Hooks.do
  isShowing /\ isShowingId <- Hooks.useState false

  Hooks.useQuery queryToken case _ of
    OpenSettings reply -> do
      Hooks.put isShowingId true
      pure (Just reply)

  let handleClick = Just do
        navigate Route.Home
        Hooks.put isShowingId false

  Hooks.pure do
    whenElem isShowing $ \_ -> Modal.modal handleClick
      [ Modal.header
        { title: Just "Settings"
        , action: handleClick
        }
      , Modal.body
        [ HH.text "This is the settings modal" ]
      , Modal.footer
        { buttons:
            [ HH.button
              [ HP.class_ (HH.ClassName "btn") 
              , HE.onClick \_ -> handleClick
              ]
              [ HH.text "Save" ]
            ]
        }
      ]
