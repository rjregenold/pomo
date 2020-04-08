module Test.Pomo.Data.TimerSettings where

import Prelude

import Data.Array as Array
import Data.Enum (class BoundedEnum, Cardinality, toEnum, enumFromTo, cardinality, succ, fromEnum, pred)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Console (log)
import Pomo.Data.PomoCount (PomoCount)
import Pomo.Data.TimerSettings as TimerSettings
import Test.Assert (assert)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  log "checking that PomoCount is bounded enum"
  checkBoundedEnum (Proxy :: Proxy tomoCount)

  log "checking that DailyGoal is bounded enum"
  checkBoundedEnum (Proxy :: Proxy TimerSettings.DailyGoal)


checkBoundedEnum :: forall e. BoundedEnum e => Proxy e -> Effect Unit
checkBoundedEnum p = do
  assert (Just (bottom :: e) == toEnum (fromEnum (bottom :: e)))
  assert (pred (bottom :: e) == Nothing)
  assert (Just (top :: e) == toEnum (fromEnum (top :: e)))
  assert (succ (top :: e) == Nothing)
  let card = unwrap (cardinality :: Cardinality e)
  assert (Array.length (enumFromTo bottom (top :: e)) == card)
