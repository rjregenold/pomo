module Pomo.Component.Forms.TimerSettingsForm where
  
import Prelude

import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number as Number
import Data.Time.Duration (Minutes)
import Data.Symbol (SProxy(..))
import DOM.HTML.Indexed.StepValue as I
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pomo.Data.PomoCount (PomoCount)
import Pomo.Data.PomoCount as PomoCount
import Pomo.Data.TimerSettings (TimerSettings)

type Settings =
  { pomoDuration :: Minutes
  , shortBreakDuration :: Minutes
  , longBreakDuration :: Minutes
  , pomosBetweenLongBreak :: PomoCount
  , pomoDailyGoal :: PomoCount
  }

data FieldError
  = InvalidNumber String
  | InvalidInt String
  | InvalidPomoCount Int

class ToText item where
  toText :: item -> String

instance toTextFieldError :: ToText FieldError where
  toText (InvalidNumber str) = "\"" <> str <> "\" is not a valid number."
  toText (InvalidInt str) = "\"" <> str <> "\" is not a valid integer."
  toText (InvalidPomoCount n) = "\"" <> show n <> "\" is not between " <> show (bottom :: PomoCount) <> " and " <> show (top :: PomoCount) <> "."

instance toTextString :: ToText String where
  toText = identity

newtype SettingsForm r f = SettingsForm (r
  ( pomoDuration :: f FieldError String Minutes
  , shortBreakDuration :: f FieldError String Minutes
  , longBreakDuration :: f FieldError String Minutes
  , pomosBetweenLongBreak :: f FieldError String PomoCount
  , pomoDailyGoal :: f FieldError String PomoCount
  ))

derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

input :: forall m. Monad m => TimerSettings -> F.Input' SettingsForm m
input timerSettings =
  { initialInputs: Just (F.wrapInputFields 
    { pomoDuration: show (unwrap timerSettings.pomoDuration)
    , shortBreakDuration: show (unwrap timerSettings.shortBreakDuration)
    , longBreakDuration: show (unwrap timerSettings.longBreakDuration)
    , pomosBetweenLongBreak: show (PomoCount.unwrap timerSettings.pomosBetweenLongBreak)
    , pomoDailyGoal: show (PomoCount.unwrap timerSettings.pomoDailyGoal)
    })
  , validators: SettingsForm
      { pomoDuration: strIsNumber >>> numberIsMinutes
      , shortBreakDuration: strIsNumber >>> numberIsMinutes
      , longBreakDuration: strIsNumber >>> numberIsMinutes
      , pomosBetweenLongBreak: strIsInt >>> intIsPomoCount
      , pomoDailyGoal: strIsInt >>> intIsPomoCount
      }
  }
  where
  strIsNumber = F.hoistFnE_ $ \str -> maybe (Left $ InvalidNumber str) Right (Number.fromString str)
  strIsInt = F.hoistFnE_ $ \str -> maybe (Left $ InvalidInt str) Right (Int.fromString str)
  numberIsMinutes = F.hoistFnE_ $ \n -> Right (wrap n)
  intIsPomoCount = F.hoistFnE_ $ \n -> maybe (Left $ InvalidPomoCount n) Right (toEnum n)

spec :: forall input m. Monad m => F.Spec' SettingsForm Settings input m
spec = F.defaultSpec { render = render, handleEvent = F.raiseResult }
  where
  render st@{ form } =
    HH.div_
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
            , HP.step $ I.Step 0.25
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
            , HP.step $ I.Step 0.25
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
            , HP.step $ I.Step 0.25
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
          , HP.step $ I.Step 1.0
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
          , HP.step $ I.Step 1.0
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
