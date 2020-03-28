module Pomo.Component.Router where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Pomo.Capability.Now (class Now)
import Pomo.Component.Utils (OpaqueSlot)
import Pomo.Data.TimerSettings (TimerSettings)
import Pomo.Page.Home as Home

type State =
  {
  }

type ChildSlots =
  ( home :: OpaqueSlot Unit
  )

component 
  :: forall q m r
   . MonadAff m
  => MonadAsk { timerSettings :: TimerSettings | r } m
  => Now m
  => H.Component HH.HTML q {} Void m
component =
  H.mkComponent
    { initialState: const {}
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render :: State -> H.ComponentHTML Unit ChildSlots m
  render _ =
     HH.slot (SProxy :: _ "home") unit Home.component {} absurd
