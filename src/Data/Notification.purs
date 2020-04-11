module Pomo.Data.Notification where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe(Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as A
import Effect.Aff.Compat as AC

data Permission
  = Granted
  | Denied
  | Default

derive instance eqPermission :: Eq Permission
derive instance genericPermission :: Generic Permission _

instance showPermission :: Show Permission where
  show = genericShow

toPermission :: String -> Maybe Permission
toPermission permission = case permission of
  "granted" -> Just Granted
  "denied" -> Just Denied
  "default" -> Just Default
  _ -> Nothing

foreign import _areNotificationsSupported :: Effect Boolean

areNotificationsSupported :: Effect Boolean
areNotificationsSupported = _areNotificationsSupported

foreign import _checkPermission :: (String -> Maybe Permission) -> Effect (Maybe Permission)

checkPermission :: Effect (Maybe Permission)
checkPermission = _checkPermission toPermission

foreign import _requestPermission :: (String -> Maybe Permission) -> AC.EffectFnAff (Maybe Permission)

requestPermission :: Aff Permission
requestPermission =
  AC.fromEffectFnAff (_requestPermission toPermission) >>= case _ of
    Just permission -> pure permission
    Nothing -> A.throwError (A.error "invalid permission type")

foreign import _createNotification :: Fn2 String String (Effect Unit)

createNotification :: String -> String -> Effect Unit
createNotification title body = runFn2 _createNotification title body
