module Pomo.Page.Home where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Pomo.Capability.LocalStorage (class LocalStorage)
import Pomo.Capability.Navigate (class Navigate)
import Pomo.Capability.Notifications (class Notifications)
import Pomo.Capability.Notifications as Notifications
import Pomo.Capability.Now (class Now, now)
import Pomo.Capability.PlaySounds (class PlaySounds, playSound)
import Pomo.Component.PomoSession as PomoSessionComponent
import Pomo.Component.HTML.Header as Header
import Pomo.Component.HTML.Utils (maybeElem)
import Pomo.Component.SettingsModal as SettingsModal
import Pomo.Data.PomoSession (PomoSession)
import Pomo.Data.PomoSession as PomoSession
import Pomo.Data.Route (Route)
import Pomo.Data.Route as Route
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)
import Pomo.Data.TimerSettings as TimerSettings
import Pomo.Env (WithEnv)
import Pomo.Web.Notification.Notification as Notification
import Web.HTML.HTMLElement (HTMLElement)

type Slot = H.Slot Query Void

type State =
  { contentBody :: Maybe HTMLElement
  , timerSettings :: TimerSettings
  , pomoSession :: PomoSession
  , currentNotification :: Maybe Notification.Notification
  , alarmUrl :: Maybe String
  , startRoute :: Maybe Route
  }

type Input =
  { route :: Maybe Route
  }

data Action
  = Init
  | Tick
  | HandlePomoSession PomoSessionComponent.Output
  | HandleSettingsModal SettingsModal.Output

data Query a
  = ShowSettings a

type ChildSlots =
  ( pomoSession :: PomoSessionComponent.Slot Unit
  , settingsModal :: SettingsModal.Slot Unit
  )

_settingsModal :: SProxy "settingsModal"
_settingsModal = SProxy

component 
  :: forall r m o
   . MonadAff m
  => MonadAsk { | WithEnv r } m
  => LocalStorage m
  => Navigate m
  => Notifications m
  => Now m
  => PlaySounds m
  => H.Component HH.HTML Query Input o m
component = 
  H.mkComponent
    { initialState: \{ route } ->
        { contentBody: Nothing
        , timerSettings: TimerSettings.defaultTimerSettings
        , pomoSession: PomoSession.defaultPomoSession
        , currentNotification: Nothing
        , alarmUrl: Nothing
        , startRoute: route
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
        [ maybeElem state.contentBody \contentBody ->
            HH.slot (SProxy :: _ "pomoSession") unit PomoSessionComponent.component { containerEl: contentBody, pomoSession: state.pomoSession, timerSettings: state.timerSettings } (Just <<< HandlePomoSession)
        ]
      , HH.slot _settingsModal unit SettingsModal.component state.timerSettings (Just <<< HandleSettingsModal)
      ]

  openSettingsModal = void $ H.query _settingsModal unit $ H.tell SettingsModal.OpenSettings

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    ShowSettings a -> do
      openSettingsModal
      pure (Just a)

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction action = case action of
    Init -> do
      mContentBody <- H.getHTMLElementRef (H.RefLabel "content-body")
      { assetUrls } <- ask
      mTimerSettings <- TimerSettings.restoreSettings
      let timerSettings = fromMaybe TimerSettings.defaultTimerSettings mTimerSettings
      currentTime <- now
      st <- H.get
      mExistingSession <- map (map $ PomoSession.tickSession timerSettings currentTime) PomoSession.restoreSession
      let initSession = PomoSession.initPomoSession timerSettings.pomoDuration
          pomoSession = fromMaybe initSession mExistingSession
          st' = st 
                { contentBody = mContentBody
                , timerSettings = timerSettings
                , pomoSession = pomoSession 
                , alarmUrl = Just assetUrls.audio.ding
                }
      H.put st'
      -- start the timer if the restored timer is running
      when (PomoSession.isTimerRunning pomoSession) (startSession st')
      -- show the settings modal if that is the start route
      case st'.startRoute of
        Just Route.Settings -> openSettingsModal
        _ -> pure unit

    Tick -> do
      st <- H.get
      pomoSession <- PomoSession.tickSessionM st.timerSettings st.pomoSession
      H.put st { pomoSession = pomoSession }
      when (not $ PomoSession.isTimerRunning pomoSession) do
        PomoSession.saveSession pomoSession
        showNotification st
        playAlarm st

    HandlePomoSession o -> case o of
      PomoSessionComponent.ToggleTimer -> do
        st <- H.get
        case st.pomoSession.currentTimer.timer of
          Timer.NotRunning _ -> startSession st
          Timer.Running _ -> do
            let pomoSession = PomoSession.stopTimer st.pomoSession st.timerSettings
            PomoSession.saveSession pomoSession
            H.put st { pomoSession = pomoSession }

    HandleSettingsModal o -> case o of
      SettingsModal.SettingsUpdated s -> do
        st <- H.get
        let pomoSession = PomoSession.applyUpdatedSettings st.pomoSession s
        TimerSettings.saveSettings s
        PomoSession.saveSession pomoSession
        H.put st { pomoSession = pomoSession
                 , timerSettings = s
                 }

    where

    showNotification st = do
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
