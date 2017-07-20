module ColorPicker.Halogen.Component where

import Prelude

import CSS as CSS
import Color as Color
import ColorPicker.Halogen.Utils.Drag as Drag
import Control.Monad.Aff.Class (class MonadAff)
import DOM.Classy.Event (preventDefault)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Condition = Idle | ColorDrag | HueDrag

type State =
  { color ∷ Maybe Color.Color
  , condition ∷ Condition
  , drag ∷ Maybe Drag.DragData
  }

type Message = Unit
data Query a
  = NoOp a
  | DragStart Drag.CursorEvent a
  | DragMove Drag.DragEvent a

type HTML = H.ComponentHTML Query
type DSL m = H.ComponentDSL State Query Message m

type PickerEffects r = Drag.DragEffects r


picker ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ H.Component HH.HTML Query Unit Message m
picker = H.component
  { initialState: const
      { color: Nothing
      , condition: Idle
      , drag: Nothing}
  , render
  , eval
  , receiver: const Nothing
  }


render ∷ State → HTML
render {color, drag} =
  HH.div
    [ HP.classes [ HH.ClassName "ColorPicker"]]
    [ HH.div
      [ HP.classes [ HH.ClassName "ColorPicker-color"]
      , HCSS.style $ CSS.backgroundColor $ pureHue $ orDefault color
      , HE.onMouseDown $ HE.input (Left >>> DragStart)
      , HE.onTouchStart $ HE.input (Right >>> DragStart)
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
    , HH.pre [HP.classes [HH.ClassName "debug"]] [HH.text $ prettyJson drag]
    ]


eval ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ Query ~> DSL m
eval (NoOp next) = pure next
eval (DragMove drag next) = do
  case drag of
    Drag.Move event dragData -> do
      H.modify _{drag = Just dragData}
    Drag.Done event -> do
      H.modify _{drag = Nothing}
  pure next
eval (DragStart event next) = do
  H.subscribe $ Drag.dragEventSource event \drag →
    Just (DragMove drag H.Listening)
  liftEff $ either preventDefault preventDefault event
  let position = Drag.cursorEventToPosition event
  let node = Drag.cursorEventToTarget event
  initialDragData <- H.liftEff $
    Drag.mkDragData { prev: position , init: position } event node
  eval $ DragMove (Drag.Move event initialDragData) next

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

foreign import prettyJson ∷ ∀ a. a → String
