module Pomo.Component.SettingsModal where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Pomo.Capability.Navigate (class Navigate, navigate)
import Pomo.Capability.Notifications (class Notifications)
import Pomo.Capability.Notifications as Notifications
import Pomo.Component.Hooks.UseInitializer (useInitializer)
import Pomo.Component.HTML.Utils (whenElem)
import Pomo.Component.Modal as Modal
import Pomo.Data.Route as Route
-- TODO: abstract this dependency away
import Pomo.Web.Notification.Notification as Notification

type Slot = H.Slot Query Void

type State =
  { modalId :: Maybe H.SubscriptionId
  , areNotificationsSupported :: Boolean
  , notificationPermission :: NotificationPermission
  }

defaultState :: State
defaultState =
  { modalId: Nothing
  , areNotificationsSupported: false
  , notificationPermission: NotAsked
  }

data NotificationPermission
  = NotAsked
  | Granted
  | Denied

derive instance eqNotificationPermission :: Eq NotificationPermission

data Action
  = CloseSettings
  | HandleKeySettings

data Query a
  = OpenSettings a

component 
  :: forall i o m
   . MonadAff m 
  => Navigate m
  => Notifications m
  => H.Component HH.HTML Query i o m
component = Hooks.component \{ queryToken } _ -> Hooks.do
  state /\ stateId <- Hooks.useState defaultState

  useInitializer do
    noteSupport <- Notifications.areNotificationsSupported
    mPermissions <- if noteSupport
                    then Notifications.checkPermission
                    else pure Nothing
    Hooks.modify_ stateId $ _ { areNotificationsSupported = noteSupport 
                              , notificationPermission = maybe NotAsked toNotificationPermission mPermissions
                              }

  let closeModal_ = do
        navigate Route.Home
        Hooks.modify_ stateId $ _ { modalId = Nothing }

      closeModal = Just closeModal_

      requestNotificationPermissions = Just do
        permission <- Notifications.requestPermission
        let p = case permission of
                  Notification.Granted -> Granted
                  _ -> Denied
        Hooks.modify_ stateId _ { notificationPermission = p }

      keyPressed ev = Just do
        { modalId } <- Hooks.get stateId
        traverse_ (\sid -> Modal.hooksWhenClose ev sid closeModal_) modalId

  Hooks.useQuery queryToken case _ of
    OpenSettings reply -> do
      id <- Modal.hooksInitWith keyPressed
      Hooks.modify_ stateId _ { modalId = Just id }
      pure (Just reply)

  Hooks.pure do
    whenElem (isJust state.modalId) \_ -> Modal.modal closeModal
      [ Modal.header
        { title: Just "Settings"
        , action: closeModal
        }
      , Modal.body
        [ whenElem (showEnableNotificationsBtn state) $ \_ ->
            HH.button
              [ HP.class_ $ HH.ClassName "btn"
              , HE.onClick \_ -> requestNotificationPermissions
              ]
              [ HH.text notificationsLabel ]
        ]
      ]

  where

  toNotificationPermission = case _ of
    Notification.Granted -> Granted
    Notification.Denied -> Denied
    Notification.Default -> NotAsked

  notificationsLabel = "Enable Notifications"

  showEnableNotificationsBtn state =
    state.areNotificationsSupported && state.notificationPermission == NotAsked
