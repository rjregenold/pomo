module Pomo.Page.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pomo.Capability.LocalStorage (class LocalStorage)
import Pomo.Capability.Now (class Now, now)
import Pomo.Data.PomoSession (PomoSession)
import Pomo.Data.PomoSession as PomoSession
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)

type State =
  { pomoSession :: PomoSession
  , forkId :: Maybe H.ForkId
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
  => LocalStorage m
  => H.Component HH.HTML q {} Void m
component = 
  H.mkComponent
    { initialState: \_ ->
        { pomoSession: PomoSession.defaultPomoSession
        , forkId: Nothing
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
      , HH.h2_ [ HH.text timerTypeLabel ]
      , HH.h3_ [ HH.text timerLabel ]
      , HH.button
          [ HP.title buttonLabel
          , HE.onClick \_ -> Just ToggleTimer 
          ]
          [ HH.text buttonLabel ]
      ]

    where

    timerTypeLabel = case state.pomoSession.currentTimer.timerType of
      PomoSession.Pomodoro -> "Pomodoro"
      PomoSession.ShortBreak -> "Short Break"
      PomoSession.LongBreak -> "Long Break"

    timerLabel = Timer.render state.pomoSession.currentTimer.timer

    buttonLabel = case state.pomoSession.currentTimer.timer of
      Timer.NotRunning _ -> "Start"
      Timer.Running _ -> "Stop"

  handleAction :: forall slots. Action -> H.HalogenM State Action slots Void m Unit
  handleAction action = case action of
    Init -> do
      { timerSettings } <- ask
      currentTime <- now
      st <- H.get
      mSession <- PomoSession.restoreSession
      let mSession' = mSession >>= \s -> PomoSession.tickSession s timerSettings currentTime
          pomoSession = fromMaybe (PomoSession.initPomoSession timerSettings.pomoDuration) mSession'
          st' = st { pomoSession = pomoSession }
      H.put st'
      case pomoSession.currentTimer.timer of
        -- start the timer if the restored timer is running
        Timer.Running _ -> startSession st'
        _ -> pure unit
    ToggleTimer -> do
      st <- H.get
      case st.pomoSession.currentTimer.timer of
        Timer.NotRunning _ -> startSession st
        Timer.Running _ -> do
          { timerSettings } <- ask
          let pomoSession = PomoSession.stopTimer st.pomoSession timerSettings
          PomoSession.saveSession pomoSession
          H.put st
            { pomoSession = pomoSession
            , forkId = Nothing
            }
          killFork st.forkId

    Tick -> do
      st <- H.get
      { timerSettings } <- ask
      mSess' <- PomoSession.tickSessionM st.pomoSession timerSettings
      for_ mSess' $ \pomoSession -> do
        let done = Timer.isComplete pomoSession.currentTimer.timer
        H.put st
          { pomoSession = pomoSession
          , forkId = if done then Nothing else st.forkId
          }
        when done $ do
           PomoSession.saveSession pomoSession
           killFork st.forkId

    where

    startSession st = do
      currentTime <- now
      let loop = do
            H.liftAff (delay tickDelay)
            handleAction Tick
            loop
      forkId <- H.fork loop
      let pomoSession = PomoSession.startTimer st.pomoSession currentTime
      PomoSession.saveSession pomoSession
      H.put st
        { pomoSession = pomoSession
        , forkId = Just forkId
        }

    tickDelay = Milliseconds 50.0

    killFork = traverse_ H.kill
