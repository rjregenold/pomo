module Pomo.Component.Forms.TimerSettingsForm where
  
import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Number as Number
import Data.Symbol (SProxy(..))
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Settings =
  { pomoDuration :: Number
  , shortBreakDuration :: Number
  , longBreakDuration :: Number
  , pomosBetweenLongBreak :: Number
  , pomoDailyGoal :: Number
  }

data FieldError
  = InvalidNumber

newtype SettingsForm r f = SettingsForm (r
  ( pomoDuration :: f FieldError String Number
  , shortBreakDuration :: f FieldError String Number
  , longBreakDuration :: f FieldError String Number
  , pomosBetweenLongBreak :: f FieldError String Number
  , pomoDailyGoal :: f FieldError String Number
  ))

derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

input :: forall m. Monad m => F.Input' SettingsForm m
input =
  { initialInputs: Just (F.wrapInputFields 
    { pomoDuration: "25"
    , shortBreakDuration: "5"
    , longBreakDuration: "15"
    , pomosBetweenLongBreak: "4"
    , pomoDailyGoal: "12" 
    })
  , validators: SettingsForm
      { pomoDuration: strIsNumber
      , shortBreakDuration: strIsNumber
      , longBreakDuration: strIsNumber
      , pomosBetweenLongBreak: strIsNumber
      , pomoDailyGoal: strIsNumber
      }
  }
  where
  strIsNumber = F.hoistFnE_ $ maybe (Left InvalidNumber) Right <<< Number.fromString

spec :: forall input m. Monad m => F.Spec' SettingsForm Settings input m
spec = F.defaultSpec { render = render, handleEvent = F.raiseResult }
  where
  render st@{ form } =
    HH.form_
      [ HH.div
        [ HP.class_ $ HH.ClassName "form-group"
        ]
        [ HH.label
          [ HP.class_ $ HH.ClassName "form-label"
          ]
          [ HH.text "Pomodoro Duration" ]
        , HH.div
          [ HP.class_ $ HH.ClassName "input-group"
          ]
          [ HH.input
            [ HP.class_ $ HH.ClassName "form-input"
            , HP.value $ F.getInput _pomoDuration form
            , HP.type_ $ HP.InputNumber
            , HE.onValueInput $ Just <<< F.set _pomoDuration
            ]
          , HH.span
            [ HP.class_ $ HH.ClassName "input-group-addon"
            ]
            [ HH.text "Minutes" ]
          ]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "form-group"
        ]
        [ HH.label
          [ HP.class_ $ HH.ClassName "form-label"
          ]
          [ HH.text "Short Break Duration" ]
        , HH.div
          [ HP.class_ $ HH.ClassName "input-group"
          ]
          [ HH.input
            [ HP.class_ $ HH.ClassName "form-input"
            , HP.value $ F.getInput _shortBreakDuration form
            , HP.type_ $ HP.InputNumber
            , HE.onValueInput $ Just <<< F.set _shortBreakDuration
            ]
          , HH.span
            [ HP.class_ $ HH.ClassName "input-group-addon"
            ]
            [ HH.text "Minutes" ]
          ]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "form-group"
        ]
        [ HH.label
          [ HP.class_ $ HH.ClassName "form-label"
          ]
          [ HH.text "Long Break Duration" ]
        , HH.div
          [ HP.class_ $ HH.ClassName "input-group"
          ]
          [ HH.input
            [ HP.class_ $ HH.ClassName "form-input"
            , HP.value $ F.getInput _longBreakDuration form
            , HP.type_ $ HP.InputNumber
            , HE.onValueInput $ Just <<< F.set _longBreakDuration
            ]
          , HH.span
            [ HP.class_ $ HH.ClassName "input-group-addon"
            ]
            [ HH.text "Minutes" ]
          ]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "form-group"
        ]
        [ HH.label
          [ HP.class_ $ HH.ClassName "form-label"
          ]
          [ HH.text "Number of Pomodoros Between Long Breaks" ]
        , HH.input
          [ HP.class_ $ HH.ClassName "form-input"
          , HP.value $ F.getInput _pomosBetweenLongBreak form
          , HP.type_ $ HP.InputNumber
          , HE.onValueInput $ Just <<< F.set _pomosBetweenLongBreak
          ]
        ]
      , HH.div
        [ HP.class_ $ HH.ClassName "form-group"
        ]
        [ HH.label
          [ HP.class_ $ HH.ClassName "form-label"
          ]
          [ HH.text "Daily Goal" ]
        , HH.input
          [ HP.class_ $ HH.ClassName "form-input"
          , HP.value $ F.getInput _pomoDailyGoal form
          , HP.type_ $ HP.InputNumber
          , HE.onValueInput $ Just <<< F.set _pomoDailyGoal
          ]
        ]
      , HH.button
        [ HP.classes $ HH.ClassName <$>
          [ "btn"
          , "btn-primary"
          ]
        , HE.onClick \_ -> Just F.submit ]
        [ HH.text "Save Changes" ]
      ]
    where
    _pomoDuration = SProxy :: SProxy "pomoDuration"
    _shortBreakDuration = SProxy :: SProxy "shortBreakDuration"
    _longBreakDuration = SProxy :: SProxy "longBreakDuration"
    _pomosBetweenLongBreak = SProxy :: SProxy "pomosBetweenLongBreak"
    _pomoDailyGoal = SProxy :: SProxy "pomoDailyGoal"
