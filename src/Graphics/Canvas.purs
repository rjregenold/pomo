module Pomo.Graphics.Canvas where

import Prelude

import Color (Color)
import Color as Color
import Data.Array as Array
import Data.Foldable (for_)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Newtype (wrap)
import Data.String as String
import Effect (Effect)
import Graphics.Canvas as Canvas
import Math as Math
import Pomo.Data.Foldable (foldlWithIndexM)

type Position =
  { x :: Number
  , y :: Number
  }

data TextAlign
  = Left
  | Right
  | Center

derive instance eqTextAlign :: Eq TextAlign

data TextPosition
  = Inside
  | Outside

derive instance eqTextPosition :: Eq TextPosition

data TextFacing
  = Inward
  | Outward

derive instance eqTextFacing :: Eq TextFacing

type CurvedTextArgs =
  { text :: String
  , startPos :: Position
  , diameter :: Number
  , startAngle :: Number
  , align :: TextAlign
  , pos :: TextPosition
  , facing :: TextFacing
  , fontName :: String
  , fontSize :: String
  , color :: Color
  , kerning :: Number
  }

defaultTextArgs :: CurvedTextArgs
defaultTextArgs =
  { text: ""
  , startPos: { x: 0.0, y: 0.0 }
  , diameter: 0.0
  , startAngle: 0.0
  , align: Center
  , pos: Inside
  , facing: Inward
  , fontName: "Arial"
  , fontSize: "12px"
  , color: Color.fromInt 0x000000
  , kerning: 0.0
  }

foreign import _setTextBaseline :: Fn2 Canvas.Context2D String (Effect Unit)

setTextBaseline :: Canvas.Context2D -> String -> Effect Unit
setTextBaseline ctx = runFn2 _setTextBaseline ctx

foreign import _getTextHeight :: Fn3 String String String (Effect Number)

getTextHeight :: String -> String -> String -> Effect Number
getTextHeight fontFamily fontSize = runFn3 _getTextHeight fontFamily fontSize

renderCurvedText :: Canvas.Context2D -> CurvedTextArgs -> Effect Unit
renderCurvedText ctx { text, startPos, diameter, startAngle, align, pos, facing, fontName, fontSize, color, kerning } = do
  textHeight <- getTextHeight fontName fontSize text

  let diameter' = if pos == Outside then diameter + textHeight * 2.0 else diameter
      startX = startPos.x + diameter' / 2.0
      startY = startPos.y + diameter' / 2.0
      shouldReverseText = ((align == Left || align == Center) && facing == Inward) || (align == Right && facing == Outward)
      text' = if shouldReverseText then reverseString text else text
      clockwise = if align == Right then 1.0 else -1.0

  Canvas.withContext ctx do
    Canvas.setFillStyle ctx (Color.toHexString color)
    Canvas.setFont ctx (fontSize <> " " <> fontName)
    Canvas.translate ctx { translateX: startX, translateY: startY }
    setTextBaseline ctx "middle"
    Canvas.setTextAlign ctx Canvas.AlignCenter

    startAngle' <- calculateStartAngle textHeight diameter' clockwise

    Canvas.rotate ctx startAngle'

    for_ (String.split (wrap "") text') \x -> do
      { width } <- Canvas.measureText ctx x
      Canvas.rotate ctx ((width / 2.0) / (diameter' / 2.0 - textHeight) * clockwise)
      let multY = if facing == Inward then 1.0 else -1.0
      Canvas.fillText ctx x 0.0 (multY * (0.0 - diameter' / 2.0 + textHeight / 2.0))
      Canvas.rotate ctx ((width / 2.0 + kerning) / (diameter' / 2.0 - textHeight) * clockwise)

  where
  textLength = String.length text

  reverseString = String.joinWith "" <<< Array.reverse <<< String.split (wrap "")

  calculateStartAngle textHeight diameter_ clockwise =
    let startAngle' = if facing == Outward
                      then startAngle * (Math.pi / 180.0) + Math.pi
                      else startAngle * (Math.pi / 180.0)
     in if align == Center
        then foldlWithIndexM (\idx acc x -> do
                        { width } <- Canvas.measureText ctx x
                        pure $ acc + ((width + (if idx == textLength - 1 then 0.0 else kerning)) / (diameter_ / 2.0 - textHeight)) / 2.0 * (-clockwise))
                        startAngle'
                        (String.split (wrap "") text)
        else pure startAngle'
