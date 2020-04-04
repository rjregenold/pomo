module Pomo.Page.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pomo.Capability.Now (class Now, now, nowDate)
import Pomo.Data.PomoSession (PomoSession)
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)

type State =
  { timerSettings :: Maybe TimerSettings
  , currentTimer :: Timer.Timer
  , forkId :: Maybe H.ForkId
  , pomoSession :: Maybe PomoSession
  }

data Action
  = Init
  | ToggleTimer
  | Tick

component 
  :: forall q r m
   . MonadAff m
  => MonadAsk { timerSettings :: TimerSettings | r } m
  => Now m
  => H.Component HH.HTML q {} Void m
component = 
  H.mkComponent
    { initialState: \_ ->
        { timerSettings: Nothing
        , currentTimer: Timer.NotRunning (Minutes 25.0)
        , forkId: Nothing
        , pomoSession: Nothing
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
    buttonLabel = case state.currentTimer of
      Timer.NotRunning _ -> "Start"
      Timer.Running _ -> "Stop"
    timerLabel = Timer.render state.currentTimer

  handleAction :: forall slots. Action -> H.HalogenM State Action slots Void m Unit
  handleAction action = case action of
    Init -> do
      { timerSettings } <- ask
      date <- nowDate
      let pomoSession =
            { date: date
            , completedPomos: bottom
            }
      H.modify_ _ { timerSettings = Just timerSettings 
                  , currentTimer = Timer.NotRunning timerSettings.pomoDuration
                  , pomoSession = Just pomoSession
                  }
    ToggleTimer -> do
      st <- H.get
      case st.currentTimer of
        Timer.NotRunning d -> do
          currentTime <- now
          let loop = do
                H.liftAff (delay tickDelay)
                handleAction Tick
                loop
          forkId <- H.fork loop
          H.put $ st
            { currentTimer = Timer.Running
              { currentTime: currentTime
              , startedAt: currentTime
              , duration: d
              }
            , forkId = Just forkId
            }
        Timer.Running _ -> stopTimer st
    Tick -> do
      st <- H.get
      timer' <- Timer.tickM st.currentTimer
      if Timer.isComplete timer'
      then stopTimer st
      else H.put (st { currentTimer = timer' })

    where

    tickDelay = Milliseconds 50.0

    stopTimer :: State -> H.HalogenM State Action slots Void m Unit
    stopTimer st = case st.currentTimer of
      Timer.NotRunning _ -> pure unit
      Timer.Running _ -> do
        H.put $ st 
                { currentTimer = Timer.NotRunning (Minutes 25.0)
                , forkId = Nothing 
                }
        maybe (pure unit) H.kill st.forkId
