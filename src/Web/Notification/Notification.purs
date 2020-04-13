module Pomo.Web.Notification.Notification where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Newtype (class Newtype, unwrap)
import Data.Maybe(Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as A
import Effect.Aff.Compat as AC

data Permission
  = Default
  | Denied
  | Granted

derive instance eqPermission :: Eq Permission

foreign import data Notification :: Type

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

foreign import _createNotification :: Fn2 String String (Effect Notification)

newtype Title = Title String

derive instance eqTitle :: Eq Title
derive instance newtypeTitle :: Newtype Title _

newtype Body = Body String

derive instance eqBody :: Eq Body
derive instance newtypeBody :: Newtype Body _

createNotification :: Title -> Body -> Effect Notification
createNotification title body = runFn2 _createNotification (unwrap title) (unwrap body)
