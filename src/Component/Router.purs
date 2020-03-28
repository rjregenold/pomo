module Pomo.Component.Router where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.DateTime.Instant (Instant)
import Data.Maybe(Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pomo.Capability.Now (class Now, now)
import Pomo.Env (Env)

type State =
  { enabled :: Boolean
  , lastActionAt :: Maybe Instant
  }

data Action = Toggle

component 
  :: forall m r q i o
   . MonadAsk { | r } m
  => Now m
  => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction })
    }

initialState :: forall i. i -> State
initialState = const { enabled: false, lastActionAt: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button
      [ HP.title label
      , HE.onClick (const (Just Toggle))
      ]
      [ HH.text label ]
    , HH.div_
      [ HH.text lastActionLabel ]
    ]
  where
        label = if state.enabled
                  then "On"
                  else "Off"
        lastActionLabel = 
          case state.lastActionAt of
               Nothing -> "You haven't clicked the button yet."
               Just x -> "You last clicked the button at " <> (show x)


handleAction 
  :: forall o m
   . Now m
  => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> do
    now' <- now
    H.modify_ \st -> st { enabled = not st.enabled, lastActionAt = Just now' }
