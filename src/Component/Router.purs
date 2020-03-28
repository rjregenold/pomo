module Pomo.Component.Router where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List as List
import Data.Maybe(Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pomo.Capability.Now (class Now, nowDateTime)
import Pomo.Data.RunEnv (RunEnv)
import Pomo.Data.RunEnv as RunEnv

type State =
  { enabled :: Boolean
  , runEnv :: RunEnv
  , lastActionAt :: Maybe DateTime
  }

data Action = Toggle

type WithRunEnv r =
  ( runEnv :: RunEnv | r )

component 
  :: forall query input m r
   . MonadAsk { runEnv :: RunEnv | r } m
  => Now m
  => H.Component HH.HTML query { | WithRunEnv input } Void m
component =
  H.mkComponent
    { initialState: \{ runEnv } -> { enabled: false, runEnv, lastActionAt: Nothing }
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction })
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.h1_ [ HH.text envLabel ]
    , HH.button
      [ HP.title label
      , HE.onClick (const (Just Toggle))
      ]
      [ HH.text label ]
    , HH.div_
      [ HH.text lastActionLabel ]
    ]
  where
        envLabel = case state.runEnv of
                        RunEnv.Dev -> "Development"
                        RunEnv.Prod -> "Production"
        label = if state.enabled
                  then "On"
                  else "Off"
        lastActionLabel = 
          case state.lastActionAt of
               Nothing -> "You haven't clicked the button yet."
               Just x -> "You last clicked the button at " <> (formatDateTime x)

        formatter = List.fromFoldable
          [ MonthShort
          , Placeholder " "
          , DayOfMonthTwoDigits
          , Placeholder ", "
          , YearFull
          , Placeholder " at "
          , Hours12
          , Placeholder ":"
          , MinutesTwoDigits
          , Placeholder ":"
          , SecondsTwoDigits
          , Meridiem
          ]

        formatDateTime = format formatter


handleAction 
  :: forall o m
   . Now m
  => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> do
    now' <- nowDateTime
    H.modify_ \st -> st { enabled = not st.enabled, lastActionAt = Just now' }
