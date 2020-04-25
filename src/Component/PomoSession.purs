module Pomo.Component.PomoSession where

import Prelude

import Color (Color)
import Color as Color
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
import Pomo.Graphics.Canvas as PomoCanvas
import Web.HTML.HTMLElement (HTMLElement)

type Input = 
  { containerEl :: HTMLElement
  , pomoSession :: PomoSession
  , timerSettings :: TimerSettings
  }

type Theme =
  { color0 :: Color
  , color1 :: Color
  , color2 :: Color
  , color3 :: Color
  , color4 :: Color
  }

defaultTheme :: Theme
defaultTheme =
  { color0: Color.fromInt 0xffffff
  , color1: Color.fromInt 0x058b00
  , color2: Color.fromInt 0xb82ee5
  , color3: Color.fromInt 0x909195
  , color4: Color.fromInt 0xffffff
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
      , HH.h2_ [ HH.text $ timerLabel input.pomoSession ]
      , HH.h5_ [ HH.text $ pomosCompletedLabel input.pomoSession ]
      ]

  where

  canvasId = "timer-canvas"

  useTimerRender deps@{ contentRect, pomoSession, timerSettings } = Hooks.captures deps Hooks.useTickEffect do
    mCanvas <- liftEffect $ Canvas.getCanvasElementById canvasId
    mContext <- traverse (liftEffect <<< Canvas.getContext2D) mCanvas
    let f = lift3 (renderTimer timerSettings pomoSession defaultTheme) mCanvas mContext contentRect
    _ <- liftEffect (sequence f)
    pure Nothing

  renderTimer timerSettings pomoSession theme canvas ctx rect = Canvas.withContext ctx do
    clearContext
    renderPomosRemaining 0
    renderText (pomosRemainingTodayLabel timerSettings pomoSession) 0
    renderPomosUntilLongBreak 1
    renderText (pomosUntilLongBreakLabel timerSettings pomoSession) 1
    renderCurrentTimer 2
    renderText (timerTypeLabel pomoSession) 2
    where
    canvasDim = Math.min rect.height rect.width
    -- make the stroke width a percentage of the canvas dimension
    strokeWidth = canvasDim * 0.035
    spaceBetweenStrokes = 0.0
    fullRadius = canvasDim / 2.0 - (strokeWidth / 2.0) - (strokeWidth + spaceBetweenStrokes)
    startX = rect.width / 2.0 - fullRadius
    startY = rect.height / 2.0 - fullRadius
    startAngle = 0.0
    endAngle = Math.pi * 2.0
    clearContext = do
      Canvas.setFillStyle ctx (Color.toHexString theme.color0)
      Canvas.fillRect ctx 
        { height: rect.height
        , width: rect.width
        , x: rect.left
        , y: rect.top 
        }
    renderPomosRemaining mult = Canvas.strokePath ctx do
      let totalTicks = Int.toNumber (unwrap timerSettings.pomoDailyGoal)
          remainingTicks = Int.toNumber (pomosRemainingToday timerSettings pomoSession)
          tickValue = endAngle / totalTicks
          end = tickValue * remainingTicks
          circle = 
            { start: startAngle
            , end: end
            , radius: fullRadius - ((strokeWidth + spaceBetweenStrokes) * Int.toNumber mult)
            , x: rect.width / 2.0
            , y: rect.height / 2.0
            }
      Canvas.setStrokeStyle ctx (Color.toHexString theme.color1)
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
            , radius: fullRadius - ((strokeWidth + spaceBetweenStrokes) * Int.toNumber mult)
            , x: rect.width / 2.0
            , y: rect.height / 2.0
            }
      Canvas.setStrokeStyle ctx (Color.toHexString theme.color3)
      Canvas.setLineCap ctx Canvas.Round
      Canvas.setLineWidth ctx strokeWidth
      Canvas.arc ctx circle

    renderPomosUntilLongBreak mult = Canvas.strokePath ctx do
      let totalTicks = unwrap timerSettings.pomosBetweenLongBreak
          remainingTicks = pomosUntilLongBreak timerSettings pomoSession
          tickValue = endAngle / Int.toNumber totalTicks
          end = tickValue * Int.toNumber remainingTicks
          circle = 
            { start: startAngle
            , end: end
            , radius: fullRadius - ((strokeWidth + spaceBetweenStrokes) * Int.toNumber mult)
            , x: rect.width / 2.0
            , y: rect.height / 2.0
            }
      Canvas.setStrokeStyle ctx (Color.toHexString theme.color2)
      Canvas.setLineCap ctx Canvas.Round
      Canvas.setLineWidth ctx strokeWidth
      Canvas.arc ctx circle

    renderText text mult = do
      let fontName = "Arial"
          fontSize = (show (Int.ceil (strokeWidth / 2.0))) <> "px"
      textHeight <- PomoCanvas.getTextHeight fontName fontSize text
      let mult' = (strokeWidth + spaceBetweenStrokes) * Int.toNumber mult
          -- only use percentage of the text height to account for the text descent (lowercase 'g' and 'y', etc)
          hsw = (strokeWidth - textHeight * 0.5) / 2.0
          radius = fullRadius + hsw
          args = PomoCanvas.defaultTextArgs 
                 { text = text
                 , color = theme.color4
                 , startPos = { x: startX - hsw + mult', y: startY - hsw + mult' }
                 , diameter = radius * 2.0 - mult' * 2.0
                 , startAngle = 90.0
                 , align = PomoCanvas.Right
                 , pos = PomoCanvas.Inside
                 , facing = PomoCanvas.Outward
                 , fontName = fontName
                 , fontSize = fontSize
                 }
      when (textHeight > 8.0) $ PomoCanvas.renderCurvedText ctx args

  timerTypeLabel pomoSession = case pomoSession.currentTimer.timerType of
    PomoSession.Pomodoro -> "Pomodoro"
    PomoSession.ShortBreak -> "Short Break"
    PomoSession.LongBreak -> "Long Break"

  timerLabel pomoSession = Timer.render pomoSession.currentTimer.timer

  pomosCompletedLabel pomoSession =
    "Completed Today: " <> show (unwrap pomoSession.completedPomos)

  pomosUntilLongBreak timerSettings pomoSession =
    unwrap timerSettings.pomosBetweenLongBreak - unwrap pomoSession.completedPomos `mod` unwrap timerSettings.pomosBetweenLongBreak

  pomosUntilLongBreakLabel timerSettings pomoSession =
    "Pomos Until Long Break: " <> show (pomosUntilLongBreak timerSettings pomoSession)

  pomosRemainingToday timerSettings pomoSession =
      unwrap timerSettings.pomoDailyGoal - unwrap pomoSession.completedPomos

  pomosRemainingTodayLabel timerSettings pomoSession =
    "Pomos Remaining Today: " <> show (pomosRemainingToday timerSettings pomoSession)
