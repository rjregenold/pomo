module Pomo.Capability.Notifications where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Maybe(Maybe)
import Pomo.Web.Notification.Notification as Notification
import Halogen (HalogenM)
import Halogen.Hooks (HookM)

class Monad m <= Notifications m where
  areNotificationsSupported :: m Boolean
  checkPermission :: m (Maybe Notification.Permission)
  requestPermission :: m Notification.Permission
  createNotification :: Notification.Title -> Notification.Body -> m Notification.Notification
  closeNotification :: Notification.Notification -> m Unit

instance notificationsHalogenM :: Notifications m => Notifications (HalogenM st act slots msg m) where
  areNotificationsSupported = lift areNotificationsSupported
  checkPermission = lift checkPermission
  requestPermission = lift requestPermission
  createNotification title = lift <<< createNotification title
  closeNotification = lift <<< closeNotification

instance notificationsHookM :: Notifications m => Notifications (HookM m) where
  areNotificationsSupported = lift areNotificationsSupported
  checkPermission = lift checkPermission
  requestPermission = lift requestPermission
  createNotification title = lift <<< createNotification title
  closeNotification = lift <<< closeNotification
