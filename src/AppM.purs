module Pomo.AppM where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.DateTime as DateTime
import Data.Maybe (fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Now
import Pomo.Capability.LocalStorage (class LocalStorage)
import Pomo.Capability.Notifications (class Notifications)
import Pomo.Capability.Now (class Now)
import Pomo.Capability.PlaySounds (class PlaySounds)
import Pomo.Data.Time (getTimeZoneOffset)
import Pomo.Env (Env)
import Pomo.Web.Audio.Audio as Audio
import Pomo.Web.Notification.Notification as Notification
import Type.Equality (class TypeEquals, from)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (clear, getItem, removeItem, setItem)

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM (asks from)

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime
  nowDateTimeLocal = liftEffect $ do
     utc <- Now.nowDateTime
     offset <- getTimeZoneOffset
     pure $ fromMaybe bottom $ DateTime.adjust offset utc

instance localStorageAppM :: LocalStorage AppM where
  getItem key = liftEffect $ getItem key =<< localStorage =<< window
  setItem key val = liftEffect $ setItem key val =<< localStorage =<< window
  removeItem key = liftEffect $ removeItem key =<< localStorage =<< window
  clear = liftEffect $ clear =<< localStorage =<< window

instance notificationsAppM :: Notifications AppM where
  areNotificationsSupported = liftEffect Notification.areNotificationsSupported
  checkPermission = liftEffect Notification.checkPermission
  requestPermission = liftAff Notification.requestPermission
  createNotification title = liftEffect <<< Notification.createNotification title
  closeNotification = liftEffect <<< Notification.closeNotification

instance playSoundsAppM :: PlaySounds AppM where
  playSound url = liftEffect (Audio.play =<< Audio.create url)
