module Pomo.Component.Router where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Pomo.Env (WithEnv)
import Pomo.Capability.LocalStorage (class LocalStorage)
import Pomo.Capability.Navigate (class Navigate, navigate)
import Pomo.Capability.Notifications (class Notifications)
import Pomo.Capability.Now (class Now)
import Pomo.Capability.PlaySounds (class PlaySounds)
import Pomo.Data.Route (Route)
import Pomo.Data.Route as Route
import Pomo.Page.Home as Home
import Routing.Duplex as RD
import Routing.Hash (getHash)

type State =
  { route :: Maybe Route
  }

data Action
  = Initialize

data Query a
  = Navigate Route a

type ChildSlots =
  ( home :: Home.Slot Unit
  )

_home :: SProxy "home"
_home = SProxy

component 
  :: forall m r o
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
    { initialState: const { route: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse Route.routeCodec) <$> H.liftEffect getHash
      navigate $ fromMaybe Route.Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      when (route /= Just dest) do
        when (dest == Route.Settings) do
          void $ H.query _home unit $ H.tell Home.ShowSettings
        H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      _ ->
        HH.slot _home unit Home.component { route } absurd
    Nothing ->
      HH.div_ [ HH.text "Page not found" ]
