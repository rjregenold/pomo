module Pomo.Page.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pomo.Capability.LocalStorage (class LocalStorage)
import Pomo.Capability.Notifications (class Notifications)
import Pomo.Capability.Notifications as Notifications
import Pomo.Capability.Now (class Now, now)
import Pomo.Component.HTML.Utils (whenElem)
import Pomo.Data.PomoSession (PomoSession)
import Pomo.Data.PomoSession as PomoSession
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)
import Pomo.Web.Notification.Notification as Notification

type State =
  { pomoSession :: PomoSession
  , forkId :: Maybe H.ForkId
  , areNotificationsSupported :: Boolean
  , notificationPermission :: NotificationPermission
  }

data Action
  = Init
  | ToggleTimer
  | Tick
  | RequestNotificationPermissions

data NotificationPermission
  = HaventAsked
  | Granted
  | Denied

derive instance eqNotificationPermission :: Eq NotificationPermission

component 
  :: forall q r m
   . MonadAff m
  => MonadAsk { timerSettings :: TimerSettings | r } m
  => Now m
  => LocalStorage m
  => Notifications m
  => H.Component HH.HTML q {} Void m
component = 
  H.mkComponent
    { initialState: \_ ->
        { pomoSession: PomoSession.defaultPomoSession
        , forkId: Nothing
        , areNotificationsSupported: false
        , notificationPermission: HaventAsked
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
      , whenElem showEnableNotificationsBtn $ \_ ->
          HH.button 
              [ HP.title "Enable Desktop Notifications"
              , HE.onClick \_ -> Just RequestNotificationPermissions
              ]
              [ HH.text "Enable Desktop Notifications" ]
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

    showEnableNotificationsBtn =
      state.areNotificationsSupported && state.notificationPermission == HaventAsked

  handleAction :: forall slots. Action -> H.HalogenM State Action slots Void m Unit
  handleAction action = case action of
    Init -> do
      { timerSettings } <- ask
      currentTime <- now
      st <- H.get
      noteSupport <- Notifications.areNotificationsSupported
      mPermissions <- if noteSupport
                      then Notifications.checkPermission
                      else pure Nothing
      mExistingSession <- map (map $ PomoSession.tickSession timerSettings currentTime) PomoSession.restoreSession
      let initSession = PomoSession.initPomoSession timerSettings.pomoDuration
          pomoSession = fromMaybe initSession mExistingSession
          st' = st 
                { pomoSession = pomoSession 
                , areNotificationsSupported = noteSupport
                , notificationPermission = maybe HaventAsked (case _ of
                                                                Notification.Granted -> Granted
                                                                Notification.Denied -> Denied
                                                                Notification.Default -> HaventAsked) mPermissions
                }
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
      pomoSession <- PomoSession.tickSessionM timerSettings st.pomoSession
      let done = not $ PomoSession.isTimerRunning pomoSession
      H.put st
        { pomoSession = pomoSession
        , forkId = if done then Nothing else st.forkId
        }
      when done $ do
         PomoSession.saveSession pomoSession
         _ <- Notifications.createNotification (wrap "Timer Complete") (wrap "The timer is complete.")
         killFork st.forkId

    RequestNotificationPermissions -> do
      permission <- Notifications.requestPermission
      let p = case permission of
                Notification.Granted -> Granted
                _ -> Denied
      H.modify_ _ { notificationPermission = p }

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
