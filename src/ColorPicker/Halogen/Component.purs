module ColorPicker.Halogen.Component where

import Prelude

import CSS as CSS
import Color as Color
import ColorPicker.Halogen.Utils.Drag as Drag
import Control.Monad.Aff.Class (class MonadAff)
import DOM.Classy.Event (preventDefault)
import Data.Either (Either(..), either)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Halogen (liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { color ∷ Color.Color -- switch to Maybe Color
  , props ∷ Props
  }

type Message = Unit

type Props =
  { mainRectWidth ∷ Int
  , hueRectWidth ∷ Int
  }

data Query a
  = SetProps Props a
  | FieldDragStart Drag.CursorEvent a
  | FieldDragMove Drag.DragEvent a
  | SliderDragStart Drag.CursorEvent a
  | SliderDragMove Drag.DragEvent a

type HTML = H.ComponentHTML Query

type DSL m = H.ComponentDSL State Query Message m

type PickerEffects r = Drag.DragEffects r

initialColor ∷ Color.Color
initialColor = Color.hsl 182.4 0.49 0.64

picker ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ H.Component HH.HTML Query Props Message m
picker = H.component
  { initialState: { color: initialColor, props: _ }
  , render
  , eval
  , receiver: HE.input SetProps
  }


render ∷ State → HTML
render {color, props} =
  HH.div
    [ HP.classes [ HH.ClassName "ColorPicker"]
    , HCSS.style do
        CSS.backgroundColor color
        CSS.color if Color.isLight color
          then CSS.black
          else CSS.white
    ]
    [ HH.div
      [ HP.classes [ HH.ClassName "ColorPicker-color"]
      , HCSS.style do
          CSS.width $ CSS.px $ toNumber props.mainRectWidth
          CSS.backgroundColor $ Color.hsl hsl.h 1.0 0.5
      , HE.onMouseDown $ HE.input (Left >>> FieldDragStart)
      , HE.onTouchStart $ HE.input (Right >>> FieldDragStart)
      ]
      [ HH.div [ HP.classes [ HH.ClassName "ColorPicker-field"]] []
      , HH.div
        [ HP.classes [ HH.ClassName "ColorPicker-fieldSelector"]
        , HCSS.style do
            CSS.display CSS.block
            CSS.left $ CSS.pct (hsl.s * 100.0)
            CSS.bottom $ CSS.pct (hsl.l * 100.0)
            CSS.backgroundColor color
        ]
        []
      ]
    , HH.div
      [ HP.classes
        [ HH.ClassName "ColorPicker-slider"]
        , HCSS.style do
            CSS.width $ CSS.px $ toNumber props.hueRectWidth
        , HE.onMouseDown $ HE.input (Left >>> SliderDragStart)
        , HE.onTouchStart $ HE.input (Right >>> SliderDragStart)
        ]
      [ HH.div
        [ HP.classes
          [ HH.ClassName "ColorPicker-sliderSelector"]
          , HCSS.style do
              CSS.top $ CSS.pct ((1.0 - hsl.h / 360.0) * 100.0)
          ]
          []
      ]
    ]
  where
  hsl = Color.toHSLA color

eval ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ Query ~> DSL m
eval (SetProps props next) = do
  H.modify _{props = props}
  pure next
eval (FieldDragMove drag next) = do
  case drag of
    Drag.Move event dragData → do
      {color} ← H.get
      let
        hsl = Color.toHSLA color
        s = dragData.progress.x
        l = 1.0 - dragData.progress.y
      H.modify _
        { color = Color.hsl hsl.h s l
        }
    Drag.Done event → pure unit
  pure next
eval (SliderDragMove drag next) = do
  case drag of
    Drag.Move event dragData → do
      {color} ← H.get
      let
        h = (1.0 - dragData.progress.y) * 360.0
        hsl = Color.toHSLA color
      H.modify _
        { color = Color.hsl h hsl.s hsl.l
        }
    Drag.Done event → pure unit
  pure next
eval (SliderDragStart event next) = startDrag SliderDragMove event next
eval (FieldDragStart event next) = startDrag FieldDragMove event next

startDrag
  ∷ ∀ a m r. MonadAff (PickerEffects r) m
  ⇒ (∀ b. Drag.DragEvent → b → Query b)
  → Drag.CursorEvent
  → a
  → DSL m a
startDrag action event next = do
  H.subscribe $ Drag.dragEventSource event \drag →
    Just (action drag H.Listening)
  liftEff $ either preventDefault preventDefault event
  initialDragData ← liftEff $ Drag.mkFirstDragData event
  eval $ action (Drag.Move event initialDragData) next
