module ColorPicker.Halogen.Component where

import Debug.Trace
import Prelude

import CSS as CSS
import Color as Color
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import DOM (DOM)
import DOM.Classy.Event (class IsEvent, target)
import DOM.Classy.HTMLElement (getBoundingClientRect)
import DOM.Classy.Node (fromNode)
import DOM.Event.MouseEvent as MouseE
import DOM.Event.TouchEvent as TouchE
import DOM.Event.Types (MouseEvent, TouchEvent)
import DOM.HTML.Types (HTMLElement)
import Data.Either (Either(..), either)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (abs)

data Condition = Idle | ColorDrag | HueDrag

type State =
  { color ∷ Maybe Color.Color
  , condition ∷ Condition
  }

type Message = Unit
data Query a
  = NoOp a
  | StartUpdateColor (Either TouchEvent MouseEvent) a
  | EndUpdateColor a
  | UpdateColor (Either TouchEvent MouseEvent) a


type HTML = H.ComponentHTML Query
type DSL m = H.ComponentDSL State Query Message m

type PickerEffects r = (dom :: DOM | r)


picker ∷ ∀ m r. MonadEff (PickerEffects r) m ⇒ H.Component HH.HTML Query Unit Message m
picker = H.component
  { initialState: const { color: Nothing, condition: Idle }
  , render
  , eval
  , receiver: const Nothing
  }


type Position = { x ∷ Number, y ∷ Number }

positionInRect
  ∷ ∀ r
  . Position
  → { left ∷ Number , width ∷ Number , top ∷ Number , height ∷ Number | r}
  → Position
positionInRect { x, y } { left, width, top, height } =
  { x: (x - left) / width * 100.0
  , y: (y - top) / height * 100.0
  }

applyRect
  ∷ ∀ event r
  . IsEvent event
  ⇒ event
  → Position
  → Eff (PickerEffects r) Position
applyRect e input =
  let
    elem ∷ Maybe HTMLElement
    elem = fromNode $ target e
    rect = getBoundingClientRect <$> elem
  in
    case rect of
      Nothing → pure input
      Just p -> positionInRect input <$> p

touchEventToPosition ∷ ∀ r. TouchEvent → Eff (PickerEffects r) Position
touchEventToPosition e =
  let
    fstTouch = e # TouchE.touches # TouchE.item 0
  in
    case fstTouch of
      Nothing → pure {x: 0.0, y: 0.0}
      Just t → applyRect e
        { x: toNumber $ TouchE.clientX t
        , y: toNumber $ TouchE.clientY t}


mouseEventToPosition ∷ ∀ r. MouseEvent → Eff (PickerEffects r) Position
mouseEventToPosition e = applyRect e
  { x: toNumber $ MouseE.clientX e
  , y: toNumber $ MouseE.clientY e}

render ∷ State → HTML
render {color} =
  HH.div
    [ HP.classes [ HH.ClassName "ColorPicker"]]
    [ HH.div
      [ HP.classes [ HH.ClassName "ColorPicker-color"]
      , HCSS.style $ CSS.backgroundColor $ pureHue $ orDefault color
      , HE.onMouseDown $ HE.input (Right >>> StartUpdateColor)
      , HE.onMouseMove $ HE.input (Right >>> UpdateColor)
      , HE.onMouseUp $ HE.input_ EndUpdateColor
      , HE.onTouchStart $ HE.input (Left >>> StartUpdateColor)
      , HE.onTouchMove $ HE.input (Left >>> UpdateColor)
      , HE.onTouchEnd $ HE.input_ EndUpdateColor
      ]
      [ HH.div [ HP.classes [ HH.ClassName "ColorPicker-saturation"]] []
      , HH.div [ HP.classes [ HH.ClassName "ColorPicker-brightness"]] []
      , HH.div
        [ HP.classes [ HH.ClassName "ColorPicker-sbSelector"]
        , HCSS.style $ case color of
            Nothing → CSS.display CSS.displayNone
            Just c → do
              CSS.backgroundColor c
              CSS.borderColor $ if Color.isLight c
                then CSS.black
                else CSS.white
        ]
        []
      ]
    , HH.div
      [ HP.classes [ HH.ClassName "ColorPicker-hue"]]
      [ HH.div
        [ HP.classes
          [ HH.ClassName "ColorPicker-hSelector"]
          , let
              hue = getHue $ orDefault color
            in
              HCSS.style do
                CSS.top $ CSS.pct (hue / 360.0 * 100.0)
          ]
          []
      ]
    ]


eval ∷ ∀ m r. MonadEff (PickerEffects r) m ⇒ Query ~> DSL m
eval (NoOp next) = pure next
eval (StartUpdateColor event next) = do
  H.modify _{condition = ColorDrag}
  pos <- H.liftEff $ posFromEvent event
  traceAnyA pos
  pure next

eval (EndUpdateColor next) = do
  H.modify _{condition = Idle}
  pure next


eval (UpdateColor event next) = do
  {condition} ← H.get
  case condition of
    ColorDrag -> do
      pos <- H.liftEff $ posFromEvent event
      traceAnyA pos
    _ -> pure unit
  pure next

posFromEvent :: ∀ r. Either TouchEvent MouseEvent -> Eff (PickerEffects r) Position
posFromEvent = either touchEventToPosition mouseEventToPosition

-- data Condition = Idle | ColorDrag | HueDrag

orDefault ∷ Maybe Color.Color → Color.Color
orDefault = fromMaybe (Color.hsl 180.0 0.0 0.0)

pureHue ∷ Color.Color → Color.Color
pureHue c = Color.hsl (getHue c) 1.0 0.5

getHue∷ Color.Color → Number
getHue = Color.toHSLA >>> _.h

getSaturation∷ Color.Color → Number
getSaturation = Color.toHSLA >>> _.s

getLightness∷ Color.Color → Number
getLightness = Color.toHSLA >>> _.l
