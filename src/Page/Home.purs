module Pomo.Page.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pomo.Capability.Now (class Now, now)
import Pomo.Data.TimerSettings (TimerSettings)

type State =
  { timerSettings :: Maybe TimerSettings
  , timerState :: TimerState
  }

type RunningTimerState =
  { startedAt :: Instant
  , forkId :: H.ForkId
  , currentTime :: Instant 
  }

data TimerState
  = NotRunning
  | Running RunningTimerState

data Action
  = Init
  | ToggleTimer
  | Tick

component 
  :: forall q m r
   . MonadAff m
  => MonadAsk { timerSettings :: TimerSettings | r } m
  => Now m
  => H.Component HH.HTML q {} Void m
component = 
  H.mkComponent
    { initialState: \_ -> 
        { timerSettings: Nothing
        , timerState: NotRunning
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
          [ HP.title buttonLabel
          , HE.onClick \_ -> Just ToggleTimer 
          ]
          [ HH.text buttonLabel ]
      ]
    where
    buttonLabel = case state.timerState of
      NotRunning -> "Start"
      Running _ -> "Stop"
    msDiff (Milliseconds a) (Milliseconds b) = a - b
    timerLabel = case state.timerState of
      NotRunning -> "25:00"
      Running ts -> show (msDiff (unInstant ts.currentTime) (unInstant ts.startedAt))

  handleAction :: forall slots. Action -> H.HalogenM State Action slots Void m Unit
  handleAction action = case action of
    Init -> do
      { timerSettings } <- ask
      H.modify_ _ { timerSettings = Just timerSettings }
    ToggleTimer -> do
      st <- H.get
      case st.timerState of
        NotRunning -> do
          currentTime <- now
          let loop = do
                H.liftAff (delay (Milliseconds 1000.0))
                handleAction Tick
                loop
          forkId <- H.fork loop
          H.put $ st
            { timerState = Running
              { currentTime: currentTime
              , startedAt: currentTime
              , forkId: forkId
              }
            }
        Running ts -> do
          H.kill ts.forkId
          H.put $ st
            { timerState = NotRunning
            }
    Tick -> do
      st <- H.get
      case st.timerState of
        NotRunning -> pure unit
        Running ts -> do
          currentTime <- now
          H.put $ st { timerState = Running $ ts { currentTime = currentTime } }
