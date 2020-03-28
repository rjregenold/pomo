module Pomo.Page.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Pomo.Data.TimerSettings (TimerSettings)

type State =
  { timerSettings :: Maybe TimerSettings
  , running :: Boolean
  , timerForkId :: Maybe H.ForkId
  , ticks :: Int
  }

data Action
  = Init
  | ToggleTimer
  | Tick

component 
  :: forall q m r
   . MonadAff m
  => MonadAsk { timerSettings :: TimerSettings | r } m
  => H.Component HH.HTML q {} Void m
component = 
  H.mkComponent
    { initialState: \_ -> 
        { timerSettings: Nothing
        , running: false
        , timerForkId: Nothing
        , ticks: 0 
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }
  where
  render :: forall slots. State -> H.ComponentHTML Action slots m
  render state =
    HH.div_
      [ HH.h1_ [ HH.text "Pomo" ]
      , HH.h3_ [ HH.text timerLabel ]
      , HH.button
          [ HE.onClick \_ -> Just ToggleTimer ]
          [ HH.text $ if state.running then "Stop" else "Start" ]
      ]
    where
    timerLabel = 
      if state.running
        then show state.ticks
        else "25:00"

  handleAction :: forall slots. Action -> H.HalogenM State Action slots Void m Unit
  handleAction action = 
    case action of
         Init -> do
            { timerSettings } <- ask
            H.modify_ _ { timerSettings = Just timerSettings }
         ToggleTimer -> do
          st <- H.modify $ \st -> st
           { running = not st.running
           , ticks = if st.running then 0 else st.ticks
           }
          if st.running
          then do
             let loop = do
                   H.liftAff (delay (Milliseconds 1000.0))
                   handleAction Tick
                   loop
             forkId <- H.fork loop
             H.modify_ _ { timerForkId = Just forkId }
          else do
             _ <- maybe (pure unit) H.kill st.timerForkId
             H.modify_ _ { timerForkId = Nothing }
         Tick -> H.modify_ \st -> st { ticks = st.ticks + 1 }

