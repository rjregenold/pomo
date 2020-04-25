module Pomo.Page.Home where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pomo.Capability.LocalStorage (class LocalStorage)
import Pomo.Capability.Notifications (class Notifications)
import Pomo.Capability.Notifications as Notifications
import Pomo.Capability.Now (class Now, now)
import Pomo.Capability.PlaySounds (class PlaySounds, playSound)
import Pomo.Component.PomoSession as PomoSessionComponent
import Pomo.Component.HTML.Utils (maybeElem, whenElem)
import Pomo.Component.Utils (OpaqueSlot)
import Pomo.Data.PomoSession (PomoSession)
import Pomo.Data.PomoSession as PomoSession
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)
import Pomo.Env (WithEnv)
import Pomo.Web.Notification.Notification as Notification
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.Window as Window

type State =
  { body :: Maybe HTMLElement
  , timerSettings :: Maybe TimerSettings
  , pomoSession :: PomoSession
  , areNotificationsSupported :: Boolean
  , notificationPermission :: NotificationPermission
  , currentNotification :: Maybe Notification.Notification
  , alarmUrl :: Maybe String
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

type ChildSlots =
  ( pomoSession :: OpaqueSlot Unit
  )

component 
  :: forall q r m
   . MonadAff m
  => MonadAsk { | WithEnv r } m
  => LocalStorage m
  => Notifications m
  => Now m
  => PlaySounds m
  => H.Component HH.HTML q {} Void m
component = 
  H.mkComponent
    { initialState: \_ ->
        { body: Nothing
        , timerSettings: Nothing
        , pomoSession: PomoSession.defaultPomoSession
        , areNotificationsSupported: false
        , notificationPermission: NotAsked
        , currentNotification: Nothing
        , alarmUrl: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

  where

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ HP.class_ (wrap "app-home")
      ]
      [ HH.div
          [ HP.class_ (wrap "app-home__content")
          ]
          [ maybeElem (lift2 (\body timerSettings -> { body, timerSettings }) state.body state.timerSettings) \{ body, timerSettings } ->
              HH.slot (SProxy :: _ "pomoSession") unit PomoSessionComponent.component { containerEl: body, pomoSession: state.pomoSession, timerSettings } absurd
          , HH.div
              [ HP.class_ (wrap "app-home__item")
              ]
              [ HH.button
                  [ HP.title buttonLabel
                  , HE.onClick \_ -> Just ToggleTimer 
                  ]
                  [ HH.text buttonLabel ]
              , whenElem showEnableNotificationsBtn $ \_ ->
                  HH.div_
                      [ HH.button 
                          [ HP.title notificationsLabel
                          , HE.onClick \_ -> Just RequestNotificationPermissions
                          ]
                          [ HH.text notificationsLabel ]
                      ]
              ]
          ]
      ]

    where

    buttonLabel = case state.pomoSession.currentTimer.timer of
      Timer.NotRunning _ -> "Start"
      Timer.Running _ -> "Stop"

    notificationsLabel = "Enable Notifications"

    showEnableNotificationsBtn =
      state.areNotificationsSupported && state.notificationPermission == NotAsked

  handleAction :: forall slots. Action -> H.HalogenM State Action slots Void m Unit
  handleAction action = case action of
    Init -> do
      mBody <- liftEffect getBody
      { assetUrls, timerSettings } <- ask
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
                { body = mBody
                , timerSettings = Just timerSettings
                , pomoSession = pomoSession 
                , areNotificationsSupported = noteSupport
                , notificationPermission = maybe NotAsked (case _ of
                                                                Notification.Granted -> Granted
                                                                Notification.Denied -> Denied
                                                                Notification.Default -> NotAsked) mPermissions
                , alarmUrl = Just assetUrls.audio.ding
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
        playAlarm st

    RequestNotificationPermissions -> do
      permission <- Notifications.requestPermission
      let p = case permission of
                Notification.Granted -> Granted
                _ -> Denied
      H.modify_ _ { notificationPermission = p }

    where

    getBody = do
      window <- HTML.window
      doc <- Window.document window
      HTMLDocument.body doc

    areNotificationsEnabled st =
      st.areNotificationsSupported && st.notificationPermission == Granted

    showNotification st =
      when (areNotificationsEnabled st) do
        let noteData = notificationData st.pomoSession
        currentNotification <- Notifications.createNotification noteData.title noteData.body
        H.modify_ _ { currentNotification = Just currentNotification }

    playAlarm st =
      traverse_ playSound st.alarmUrl

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

    tickDelay = Milliseconds 33.3

    notificationData pomoSession =
      case pomoSession.currentTimer.timerType of
        PomoSession.Pomodoro -> { title: wrap "Pomodoro Complete", body: wrap "The pomodoro is complete." }
        PomoSession.ShortBreak -> { title: wrap "Short Break Complete", body: wrap "The short break is complete." }
        PomoSession.LongBreak -> { title: wrap "Long Break Complete", body: wrap "The long break is complete." }
