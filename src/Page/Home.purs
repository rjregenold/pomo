module Pomo.Page.Home where

import Prelude

import Control.Apply (lift2)
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
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)
import Pomo.Env (WithEnv)
import Pomo.Web.Notification.Notification as Notification
import Web.HTML.HTMLElement (HTMLElement)

type Slot = H.Slot Query Void

type State =
  { contentBody :: Maybe HTMLElement
  , timerSettings :: Maybe TimerSettings
  , pomoSession :: PomoSession
  , currentNotification :: Maybe Notification.Notification
  , alarmUrl :: Maybe String
  }

data Action
  = Init
  | Tick
  | HandlePomoSession PomoSessionComponent.Output

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
  => H.Component HH.HTML Query {} o m
component = 
  H.mkComponent
    { initialState: \_ ->
        { contentBody: Nothing
        , timerSettings: Nothing
        , pomoSession: PomoSession.defaultPomoSession
        , currentNotification: Nothing
        , alarmUrl: Nothing
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
        ]
      , HH.slot _settingsModal unit SettingsModal.component unit absurd
      ]

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    ShowSettings a -> do
      void $ H.query _settingsModal unit $ H.tell $ SettingsModal.OpenSettings
      pure (Just a)

  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction action = case action of
    Init -> do
      mContentBody <- H.getHTMLElementRef (H.RefLabel "content-body")
      { assetUrls, timerSettings } <- ask
      currentTime <- now
      st <- H.get
      mExistingSession <- map (map $ PomoSession.tickSession timerSettings currentTime) PomoSession.restoreSession
      let initSession = PomoSession.initPomoSession timerSettings.pomoDuration
          pomoSession = fromMaybe initSession mExistingSession
          st' = st 
                { contentBody = mContentBody
                , timerSettings = Just timerSettings
                , pomoSession = pomoSession 
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
