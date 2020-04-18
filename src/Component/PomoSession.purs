module Pomo.Component.PomoSession where

import Prelude

import Control.Apply (lift3)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration as Duration
import Data.Traversable (sequence, traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Math as Math
import Pomo.Component.Hooks.UseResizeObserver (useResizeObserver)
import Pomo.Data.PomoSession (PomoSession)
import Pomo.Data.PomoSession as PomoSession
import Pomo.Data.Timer as Timer
import Pomo.Data.TimerSettings (TimerSettings)
import Web.HTML.HTMLElement (HTMLElement)

type Input = 
  { containerEl :: HTMLElement
  , pomoSession :: PomoSession
  , timerSettings :: TimerSettings
  }

component 
  :: forall q o m
   . MonadAff m
  => H.Component HH.HTML q Input o m
component = Hooks.component \input -> Hooks.do
  contentRect <- useResizeObserver input.containerEl

  _ <- useTimerRender { contentRect, pomoSession: input.pomoSession, timerSettings: input.timerSettings }

  Hooks.pure do
    HH.div
      [ HP.class_ (wrap "app-home__item")
      ]
      [ HH.canvas 
          [ HP.id_ canvasId
          , HP.class_ (wrap "timer-details__canvas")
          , HP.width $ fromMaybe 0 $ map (_.width >>> Int.round) contentRect
          , HP.height $ fromMaybe 0 $ map (_.height >>> Int.round) contentRect
          ]
      , HH.div [ HP.class_ (wrap "timer-details__type") ] [ HH.text $ timerTypeLabel input.pomoSession ]
      , HH.div [ HP.class_ (wrap "timer-details__timer") ] [ HH.text $ timerLabel input.pomoSession ]
      , HH.div [ HP.class_ (wrap "timer-details__completed") ] [ HH.text $ pomosCompletedLabel input.pomoSession ]
      ]

  where

  canvasId = "timer-canvas"

  useTimerRender deps@{ contentRect, pomoSession, timerSettings } = Hooks.captures deps Hooks.useTickEffect do
    mCanvas <- liftEffect $ Canvas.getCanvasElementById canvasId
    mContext <- traverse (liftEffect <<< Canvas.getContext2D) mCanvas
    let f = lift3 (renderTimer timerSettings pomoSession) mCanvas mContext contentRect
    _ <- liftEffect (sequence f)
    pure Nothing

  renderTimer timerSettings pomoSession canvas ctx rect = Canvas.withContext ctx do
    clearContext
    renderPomosRemaining 0
    renderPomosUntilLongBreak 1
    renderCurrentTimer 2
    where
    backgroundColor = "#ffffff"
    pomosRemainingColor = "#058b00"
    pomosUntilLongBreakColor = "#b82ee5"
    currentTimerColor = "#909195"
    strokeWidth = 30.0
    fullRadius = (Math.min rect.height rect.width) / 2.0 - (strokeWidth / 2.0)
    startAngle = 0.0
    endAngle = Math.pi * 2.0
    clearContext = do
      Canvas.setFillStyle ctx backgroundColor
      Canvas.fillRect ctx 
        { height: rect.height
        , width: rect.width
        , x: rect.left
        , y: rect.top 
        }
    renderPomosRemaining mult = Canvas.strokePath ctx do
      let totalTicks = Int.toNumber (unwrap timerSettings.pomoDailyGoal)
          remainingTicks = totalTicks - Int.toNumber (unwrap pomoSession.completedPomos)
          tickValue = endAngle / totalTicks
          end = tickValue * remainingTicks
          circle = 
            { start: startAngle
            , end: end
            , radius: fullRadius - (strokeWidth * Int.toNumber mult)
            , x: rect.width / 2.0
            , y: rect.height / 2.0
            }
      Canvas.setStrokeStyle ctx pomosRemainingColor
      Canvas.setLineCap ctx Canvas.Round
      Canvas.setLineWidth ctx strokeWidth
      Canvas.arc ctx circle
    renderCurrentTimer mult = Canvas.strokePath ctx do
      let totalTicks = unwrap (Duration.fromDuration (Timer.timerDuration pomoSession.currentTimer.timer))
          remainingTicks = unwrap (Timer.remainingMs pomoSession.currentTimer.timer)
          tickValue = endAngle / totalTicks
          end = tickValue * remainingTicks
          circle = 
            { start: startAngle
            , end: end
            , radius: fullRadius - (strokeWidth * Int.toNumber mult)
            , x: rect.width / 2.0
            , y: rect.height / 2.0
            }
      Canvas.setStrokeStyle ctx currentTimerColor
      Canvas.setLineCap ctx Canvas.Round
      Canvas.setLineWidth ctx strokeWidth
      Canvas.arc ctx circle
    renderPomosUntilLongBreak mult = Canvas.strokePath ctx do
      let totalTicks = unwrap timerSettings.pomosBetweenLongBreak
          remainingTicks = unwrap timerSettings.pomosBetweenLongBreak - unwrap pomoSession.completedPomos `mod` unwrap timerSettings.pomosBetweenLongBreak
          tickValue = endAngle / Int.toNumber totalTicks
          end = tickValue * Int.toNumber remainingTicks
          circle = 
            { start: startAngle
            , end: end
            , radius: fullRadius - (strokeWidth * Int.toNumber mult)
            , x: rect.width / 2.0
            , y: rect.height / 2.0
            }
      Canvas.setStrokeStyle ctx pomosUntilLongBreakColor
      Canvas.setLineCap ctx Canvas.Round
      Canvas.setLineWidth ctx strokeWidth
      Canvas.arc ctx circle

  timerTypeLabel pomoSession = case pomoSession.currentTimer.timerType of
    PomoSession.Pomodoro -> "Pomodoro"
    PomoSession.ShortBreak -> "Short Break"
    PomoSession.LongBreak -> "Long Break"

  timerLabel pomoSession = Timer.render pomoSession.currentTimer.timer

  pomosCompletedLabel pomoSession =
    "Pomodoros Completed Today: " <> show (unwrap pomoSession.completedPomos)
