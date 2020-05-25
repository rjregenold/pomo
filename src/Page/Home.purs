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
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pomo.Capability.LocalStorage (class LocalStorage)
import Pomo.Capability.Navigate (class Navigate, navigate)
import Pomo.Capability.Notifications (class Notifications)
import Pomo.Capability.Notifications as Notifications
import Pomo.Capability.Now (class Now, now)
import Pomo.Capability.PlaySounds (class PlaySounds, playSound)
import Pomo.Component.Modal as Modal
import Pomo.Component.PomoSession as PomoSessionComponent
import Pomo.Component.HTML.Header as Header
import Pomo.Component.HTML.Utils (maybeElem, whenElem)
import Pomo.Component.Utils (OpaqueSlot)
import Pomo.Data.PomoSession (PomoSession)
import Pomo.Data.PomoSession as PomoSession
import Pomo.Data.Route as Route
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)
import Pomo.Env (WithEnv)
import Pomo.Web.Notification.Notification as Notification
import Web.HTML.HTMLElement (HTMLElement)
import Web.UIEvent.KeyboardEvent as KE

type Slot = H.Slot Query Void

type State =
  { contentBody :: Maybe HTMLElement
  , timerSettings :: Maybe TimerSettings
  , pomoSession :: PomoSession
  , areNotificationsSupported :: Boolean
  , notificationPermission :: NotificationPermission
  , currentNotification :: Maybe Notification.Notification
  , alarmUrl :: Maybe String
  , settingsModalId :: Maybe H.SubscriptionId
  }

data Action
  = Init
  | Tick
  | RequestNotificationPermissions
  | HandlePomoSession PomoSessionComponent.Output
  | OpenSettings
  | CloseSettings
  | HandleKeySettings KE.KeyboardEvent

data Query a
  = ShowSettings a

data NotificationPermission
  = NotAsked
  | Granted
  | Denied

derive instance eqNotificationPermission :: Eq NotificationPermission

type ChildSlots =
  ( pomoSession :: PomoSessionComponent.Slot Unit
  , settingsModal :: OpaqueSlot Unit
  )

component 
  :: forall q r m o
   . MonadAff m
  => MonadAsk { | WithEnv r } m
  => LocalStorage m
  => Navigate m
  => Notifications m
  => Now m
  => PlaySounds m
  => H.Component HH.HTML Query {} o m
component = 
  H.mkComponent
    { initialState: \_ ->
        { contentBody: Nothing
        , timerSettings: Nothing
        , pomoSession: PomoSession.defaultPomoSession
        , areNotificationsSupported: false
        , notificationPermission: NotAsked
        , currentNotification: Nothing
        , alarmUrl: Nothing
        , settingsModalId: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Init
        }
    }

  where

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ HP.classes $ HH.ClassName <$>
        [ "container"
        , "grid-lg"
        , "d-flex"
        , "flex-container-flow-col"
        , "h-100p"
        ]
      ]
      [ Header.header
      , HH.div
        [ HP.classes $ HH.ClassName <$>
          [ "d-flex"
          , "flex-item-fill"
          ]
        , HP.ref $ H.RefLabel "content-body"
        ]
        [ maybeElem (lift2 (\contentBody timerSettings -> { contentBody, timerSettings }) state.contentBody state.timerSettings) \{ contentBody, timerSettings } ->
            HH.slot (SProxy :: _ "pomoSession") unit PomoSessionComponent.component { containerEl: contentBody, pomoSession: state.pomoSession, timerSettings } (Just <<< HandlePomoSession)
        {-
        , HH.div
          [ HP.class_ (wrap "app-home__item")
          ]
          [ HH.button
            [ HP.title "Settings"
            , HP.class_ (wrap "btn")
            , HE.onClick \_ -> Just OpenSettings
            ]
            [ HH.text "Settings" ]
          , whenElem showEnableNotificationsBtn $ \_ ->
              HH.div_
                [ HH.button 
                  [ HP.title notificationsLabel
                  , HP.class_ (wrap "btn")
                  , HE.onClick \_ -> Just RequestNotificationPermissions
                  ]
                  [ HH.text notificationsLabel ]
                ]
          ]
          -}
        ]
      , maybeElem state.settingsModalId \_ -> renderSettingsModal
      ]

    where

    notificationsLabel = "Enable Notifications"

    showEnableNotificationsBtn =
      state.areNotificationsSupported && state.notificationPermission == NotAsked

    renderSettingsModal = Modal.modal CloseSettings
      [ Modal.header
        { title: Just "Settings"
        , action: Just CloseSettings
        }
      , Modal.body
        [ HH.text "This is the settings modal" ]
      , Modal.footer
        { buttons:
            [ HH.button
              [ HP.class_ (HH.ClassName "btn") 
              , HE.onClick (const (Just CloseSettings))
              ]
              [ HH.text "Save" ]
            ]
        }
      ]

  handleQuery :: forall slots a. Query a -> H.HalogenM State Action slots o m (Maybe a)
  handleQuery = case _ of
    ShowSettings a -> do
      handleAction OpenSettings
      pure (Just a)

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction action = case action of
    Init -> do
      mContentBody <- H.getHTMLElementRef (H.RefLabel "content-body")
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
                { contentBody = mContentBody
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

    HandlePomoSession o -> case o of
      PomoSessionComponent.ToggleTimer -> do
        st <- H.get
        case st.pomoSession.currentTimer.timer of
          Timer.NotRunning _ -> startSession st
          Timer.Running _ -> do
            { timerSettings } <- ask
            let pomoSession = PomoSession.stopTimer st.pomoSession timerSettings
            PomoSession.saveSession pomoSession
            H.put st { pomoSession = pomoSession }

    OpenSettings -> do
      id <- Modal.initializeWith (Just <<< HandleKeySettings)
      H.modify_ _ { settingsModalId = Just id }

    CloseSettings -> do
      navigate Route.Home
      H.modify_ _ { settingsModalId = Nothing }

    HandleKeySettings ev -> do
      { settingsModalId } <- H.get
      traverse_ (\sid -> Modal.whenClose ev sid $ handleAction CloseSettings) settingsModalId

    where

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
