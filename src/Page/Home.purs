module Pomo.Page.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
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
  , areNotificationsSupported :: Boolean
  , notificationPermission :: NotificationPermission
  , currentNotification :: Maybe Notification.Notification
  }

data Action
  = Init
  | ToggleTimer
  | Tick
  | RequestNotificationPermissions

data NotificationPermission
  = NotAsked
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
        , areNotificationsSupported: false
        , notificationPermission: NotAsked
        , currentNotification: Nothing
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
              [ HP.title notificationsLabel
              , HE.onClick \_ -> Just RequestNotificationPermissions
              ]
              [ HH.text notificationsLabel ]
      , HH.h4_ [ HH.text pomosCompletedLabel ]
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

    notificationsLabel = "Enable Desktop Notifications"

    showEnableNotificationsBtn =
      state.areNotificationsSupported && state.notificationPermission == NotAsked

    pomosCompletedLabel =
      "Pomodoros Completed Today: " <> show (unwrap state.pomoSession.completedPomos)

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
                , notificationPermission = maybe NotAsked (case _ of
                                                                Notification.Granted -> Granted
                                                                Notification.Denied -> Denied
                                                                Notification.Default -> NotAsked) mPermissions
                }
      H.put st'
      -- start the timer if the restored timer is running
      when (PomoSession.isTimerRunning pomoSession) (startSession st')

    ToggleTimer -> do
      st <- H.get
      case st.pomoSession.currentTimer.timer of
        Timer.NotRunning _ -> startSession st
        Timer.Running _ -> do
          { timerSettings } <- ask
          let pomoSession = PomoSession.stopTimer st.pomoSession timerSettings
          PomoSession.saveSession pomoSession
          H.put st { pomoSession = pomoSession }

    Tick -> do
      st <- H.get
      { timerSettings } <- ask
      pomoSession <- PomoSession.tickSessionM timerSettings st.pomoSession
      H.put st { pomoSession = pomoSession }
      when (not $ PomoSession.isTimerRunning pomoSession) do
        PomoSession.saveSession pomoSession
        showNotification st

    RequestNotificationPermissions -> do
      permission <- Notifications.requestPermission
      let p = case permission of
                Notification.Granted -> Granted
                _ -> Denied
      H.modify_ _ { notificationPermission = p }

    where

    areNotificationsEnabled st =
      st.areNotificationsSupported && st.notificationPermission == Granted

    showNotification st =
      when (areNotificationsEnabled st) do
        let noteData = notificationData st.pomoSession
        currentNotification <- Notifications.createNotification noteData.title noteData.body
        H.modify_ _ { currentNotification = Just currentNotification }

    startSession st = do
      currentTime <- now
      traverse_ Notifications.closeNotification st.currentNotification
      let loop = do
            H.liftAff (delay tickDelay)
            loopSt <- H.get
            when (PomoSession.isTimerRunning loopSt.pomoSession) do
              handleAction Tick
              loop
      _ <- H.fork loop
      let pomoSession = PomoSession.startTimer st.pomoSession currentTime
      PomoSession.saveSession pomoSession
      H.put st
        { pomoSession = pomoSession
        , currentNotification = Nothing
        }

    tickDelay = Milliseconds 250.0

    notificationData pomoSession =
      case pomoSession.currentTimer.timerType of
        PomoSession.Pomodoro -> { title: wrap "Pomodoro Complete", body: wrap "The pomodoro is complete." }
        PomoSession.ShortBreak -> { title: wrap "Short Break Complete", body: wrap "The short break is complete." }
        PomoSession.LongBreak -> { title: wrap "Long Break Complete", body: wrap "The long break is complete." }
