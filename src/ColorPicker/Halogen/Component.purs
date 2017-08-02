module ColorPicker.Halogen.Component where

import Prelude

import CSS as CSS
import Color as Color
import ColorPicker.Halogen.Utils.Drag as Drag
import Control.Monad.Aff.Class (class MonadAff)
import DOM.Classy.Event (preventDefault)
import Data.Either (Either(..), either)
import Data.Either.Nested as Either
import Data.Foldable (fold, for_)
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Int (floor, toNumber)
import Data.String as String
import Data.Maybe (Maybe(..))
import Halogen (liftEff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (halt)
import Math (round)
import NumberInput.Halogen.Component as Num
import NumberInput.Range (Range(..))
import PatternInput.Halogen.Component as PatternInput

type State =
  { color ∷ Color.Color -- switch to Maybe Color
  , props ∷ Props
  }

type Message = Unit

componentHue :: Array ColorComponent
componentHue = [Hue]

componentSL :: Array ColorComponent
componentSL = [HSL_S, HSL_L]

componentSV :: Array ColorComponent
componentSV = [HSV_S, HSV_V]

componentRGB :: Array ColorComponent
componentRGB = [Red, Green, Blue]

componentHEX :: Array ColorComponent
componentHEX = [HEX]

type ColorComponentGroups = Array ColorComponents
type ColorComponents = Array ColorComponent

type Props =
  { fieldRectWidth ∷ Int
  , sliderRectWidth ∷ Int
  , editing ∷ ColorComponentGroups
  }

data ColorComponent = Hue | HSL_S | HSV_S | HSL_L | HSV_V | Red | Green | Blue | HEX
derive instance colorComponentEq ∷ Eq ColorComponent
derive instance colorComponentOrd ∷ Ord ColorComponent

data Query a
  = SetProps Props a
  | FieldDragStart Drag.CursorEvent a
  | FieldDragMove Drag.DragEvent a
  | SliderDragStart Drag.CursorEvent a
  | SliderDragMove Drag.DragEvent a
  | ComponentUpdate (Color.Color -> Maybe Color.Color) a
  | Init a

type ChildQuery = Coproduct.Coproduct2 (Num.Query Number) (PatternInput.Query Color.Color)
type Slot = Either.Either2 ColorComponent Unit

cpColorComponent ∷ CP.ChildPath (Num.Query Number) ChildQuery ColorComponent Slot
cpColorComponent = CP.cp1

cpColorComponentHex ∷ CP.ChildPath (PatternInput.Query Color.Color) ChildQuery Unit Slot
cpColorComponentHex = CP.cp2

type HTML m = H.ParentHTML Query ChildQuery Slot m

type DSL m = H.ParentDSL State Query ChildQuery Slot Message m

type PickerEffects r = Drag.DragEffects r

initialColor ∷ Color.Color
initialColor = Color.hsl 180.0 0.5 0.5

picker ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ H.Component HH.HTML Query Props Message m
picker = H.lifecycleParentComponent
  { initialState: { color: initialColor, props: _ }
  , render
  , eval
  , receiver: HE.input SetProps
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }

render ∷ ∀ m. State → HTML m
render {color, props} =
  HH.div
    [ HP.classes [ HH.ClassName "ColorPicker"]
    , HCSS.style do
        -- TODO remove backgroundColor
        CSS.backgroundColor color
        CSS.color if Color.isLight color
          then CSS.black
          else CSS.white
    ]
    [ dragger, editing ]
  where
  hsv = Color.toHSVA color
  dragger =
    HH.div
      [ HP.classes [ HH.ClassName "ColorPicker-dragger"] ]
      [ field, slider ]

  field =
    HH.div
      [ HP.classes [ HH.ClassName "ColorPicker-field"]
      , HCSS.style do
          CSS.width $ CSS.px $ toNumber props.fieldRectWidth
          CSS.backgroundColor $ Color.hsl hsv.h 1.0 0.5
      , HE.onMouseDown $ HE.input (Left >>> FieldDragStart)
      , HE.onTouchStart $ HE.input (Right >>> FieldDragStart)
      ]
      [ HH.div [ HP.classes [ HH.ClassName "ColorPicker-fieldGradient"]] []
      , HH.div
        [ HP.classes [ HH.ClassName "ColorPicker-fieldSelector"]
        , HCSS.style do
            CSS.display CSS.block
            CSS.left $ CSS.pct (hsv.s * 100.0)
            CSS.bottom $ CSS.pct (hsv.v * 100.0)
            CSS.backgroundColor color
        ]
        []
      ]

  slider =
    HH.div
      [ HP.classes
        [ HH.ClassName "ColorPicker-slider"]
        , HCSS.style do
            CSS.width $ CSS.px $ toNumber props.sliderRectWidth
        , HE.onMouseDown $ HE.input (Left >>> SliderDragStart)
        , HE.onTouchStart $ HE.input (Right >>> SliderDragStart)
        ]
      [ HH.div
        [ HP.classes
          [ HH.ClassName "ColorPicker-sliderSelector"]
          , HCSS.style do
              CSS.top $ CSS.pct ((1.0 - hsv.h / 360.0) * 100.0)
          ]
          []
      ]

  editing =
    HH.div
      [ HP.classes [ HH.ClassName "ColorPicker-editing"] ]
      (renderEditingItem <$> props.editing )

renderEditingItem :: ∀ m. ColorComponents  -> HTML m
renderEditingItem x = HH.div [ HP.classes [ HH.ClassName "ColorPicker-editingItem"] ] $ x <#> case _ of
  Hue   -> embedNum hasValRound Hue   confHue
  HSV_S -> embedNum hasValRound HSV_S confSaturation
  HSV_V -> embedNum hasValRound HSV_V confValue
  HSL_S -> embedNum hasValRound HSL_S confSaturation
  HSL_L -> embedNum hasValRound HSL_L confLightness
  Red   -> embedNum hasvalCail  Red   confRed
  Green -> embedNum hasvalCail  Green confGreen
  Blue  -> embedNum hasvalCail  Blue  confBlue
  HEX   -> renderHex

  where
  hasValRound :: Num.HasNumberInputVal Number
  hasValRound = Num.numberHasNumberInputVal
    {fromString = Num.numberHasNumberInputVal.fromString >>> map roundFractionalNum}

  hasvalCail :: Num.HasNumberInputVal Number
  hasvalCail = Num.numberHasNumberInputVal
    {fromString = Num.numberHasNumberInputVal.fromString >>> map roundNum}

  embedNum :: Num.HasNumberInputVal Number -> ColorComponent -> Num.Config Number -> HTML m
  embedNum hasVal component conf = input conf.placeholder
    $ HH.slot' cpColorComponent component (Num.input hasVal conf) unit
    $ HE.input \(Num.NotifyChange val) -> ComponentUpdate $ \color ->
        val <#> \n -> do
          case component of
            Hue   -> modifyHSL (_{h = n}) color
            HSL_S -> modifyHSL (_{s = n / 100.0}) color
            HSL_L -> modifyHSL (_{l = n / 100.0}) color
            HSV_S -> modifyHSV (_{s = n / 100.0}) color
            HSV_V -> modifyHSV (_{v = n / 100.0}) color
            Red   -> modifyRGB (_{r = asInt n}) color
            Green -> modifyRGB (_{g = asInt n}) color
            Blue  -> modifyRGB (_{b = asInt n}) color
            HEX   -> color -- hex values will not be comming from Number input
    where
    asInt = floor
  input :: String -> HTML m -> HTML m
  input label child =
    HH.label [HP.classes [HH.ClassName "ColorPicker-input"]]
      [ HH.span [HP.classes [HH.ClassName "ColorPicker-inputLabel"]] [HH.text label]
      , child
      ]
  renderHex = input "#"
    $ HH.slot' cpColorComponentHex unit (PatternInput.input
      { fromString: \str -> Color.fromHexString $ "#" <> str
      , toString: \color -> String.toUpper $ String.drop 1 $ Color.toHexString color
      }
      { title: "Hex"
      , placeholder: "HEX"
      , root: [HH.ClassName "ColorPicker-inputElem", HH.ClassName "ColorPicker-inputElem--color"]
      , rootInvalid: [HH.ClassName "ColorPicker-inputElem--invalid"]
      }) unit
    $ HE.input \(PatternInput.NotifyChange val) -> ComponentUpdate $ const val


mkConf ∷ ∀ a. { title ∷ String, placeholder ∷ String, range ∷ Range a } -> Num.Config a
mkConf { title, placeholder, range } =
  { title
  , placeholder
  , range
  , root: [HH.ClassName "ColorPicker-inputElem"]
  , rootInvalid: [HH.ClassName "ColorPicker-inputElem--invalid"]
  , rootLength: \n -> [HH.ClassName $ "ColorPicker-inputElem--length-" <> show n]
  }

confRed ∷ Num.Config Number
confRed = mkConf
  { title: "Red"
  , placeholder: "R"
  , range: MinMax 0.0 256.0
  }

confGreen ∷ Num.Config Number
confGreen = mkConf
  { title: "Green"
  , placeholder: "G"
  , range: MinMax 0.0 256.0
  }

confBlue ∷ Num.Config Number
confBlue = mkConf
  { title: "Blue"
  , placeholder: "B"
  , range: MinMax 0.0 256.0
  }

confHue ∷ Num.Config Number
confHue = mkConf
  { title: "Hue"
  , placeholder: "H"
  , range: MinMax 0.0 360.0
  }
confSaturation ∷ Num.Config Number
confSaturation = mkConf
  { title: "Saturation"
  , placeholder: "S"
  , range: MinMax 0.0 100.0
  }
confLightness ∷ Num.Config Number
confLightness = mkConf
  { title: "Lightness"
  , placeholder: "L"
  , range: MinMax 0.0 100.0
  }

confValue ∷ Num.Config Number
confValue = mkConf
  { title: "Value"
  , placeholder: "V"
  , range: MinMax 0.0 100.0
  }



propagate :: ∀ m. DSL m Unit
propagate = do
  { color, props: { editing }} <- H.get
  let hsl = Color.toHSLA color
  let hsv = Color.toHSVA color
  let rgb = Color.toRGBA color
  for_ (fold editing) $ case _ of
    Hue   -> H.query' cpColorComponent Hue   (set $ roundFractionalNum hsl.h) >>= mustBeMounted
    HSL_S -> H.query' cpColorComponent HSL_S (set $ roundFractionalNum $ 100.0 * hsl.s) >>= mustBeMounted
    HSL_L -> H.query' cpColorComponent HSL_L (set $ roundFractionalNum $ 100.0 * hsl.l) >>= mustBeMounted
    HSV_S -> H.query' cpColorComponent HSV_S (set $ roundFractionalNum $ 100.0 * hsv.s) >>= mustBeMounted
    HSV_V -> H.query' cpColorComponent HSV_V (set $ roundFractionalNum $ 100.0 * hsv.v) >>= mustBeMounted
    Red   -> H.query' cpColorComponent Red   (set $ roundNum $ toNumber rgb.r) >>= mustBeMounted
    Green -> H.query' cpColorComponent Green (set $ roundNum $ toNumber rgb.g) >>= mustBeMounted
    Blue  -> H.query' cpColorComponent Blue  (set $ roundNum $ toNumber rgb.b) >>= mustBeMounted
    HEX   -> H.query' cpColorComponentHex unit (H.action $ PatternInput.SetValue $ Just $ color) >>= mustBeMounted
  where
  set n = H.action (Num.SetValue $ Just $ roundFractionalNum n)


eval ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ Query ~> DSL m
eval (Init next) = do
  propagate
  pure next
eval (ComponentUpdate update next) = do
  { color } <- H.get
  for_ (update color) $ \color' -> do
    H.modify _{ color = color'}
    propagate
  pure next


eval (SetProps props next) = do
  H.modify _{props = props}
  pure next
eval (FieldDragMove drag next) = do
  case drag of
    Drag.Move event dragData → do
      {color} ← H.get
      let
        hsv = Color.toHSVA color
        s = roundFractionalNum $ dragData.progress.x
        v = roundFractionalNum $ 1.0 - dragData.progress.y
      H.modify _
        { color = Color.hsv hsv.h s v
        }
    Drag.Done event → pure unit
  propagate
  pure next
eval (SliderDragMove drag next) = do
  case drag of
    Drag.Move event dragData → do
      {color} ← H.get
      let
        h = roundFractionalNum $ (1.0 - dragData.progress.y) * 360.0
        hsl = Color.toHSLA color
      H.modify _
        { color = Color.hsl h hsl.s hsl.l
        }
    Drag.Done event → pure unit
  propagate
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

type RecordHSLA = { h ∷ Number, s ∷ Number, l ∷ Number, a ∷ Number }
type RecordHSVA = { h ∷ Number, s ∷ Number, v ∷ Number, a ∷ Number }
type RecordRGBA = { r ∷ Int, g ∷ Int, b ∷ Int, a ∷ Number }

modifyHSL ∷ (RecordHSLA → RecordHSLA) -> Color.Color -> Color.Color
modifyHSL f c = case f (Color.toHSLA c) of {h, s, l, a} -> Color.hsla h s l a

modifyHSV ∷ (RecordHSVA → RecordHSVA) -> Color.Color -> Color.Color
modifyHSV f c = case f (Color.toHSVA c) of {h, s, v, a} -> Color.hsva h s v a

modifyRGB ∷ (RecordRGBA → RecordRGBA) -> Color.Color -> Color.Color
modifyRGB f c = case f (Color.toRGBA c) of {r, g, b, a} -> Color.rgba r g b a

mustBeMounted ∷ ∀ s f g p o m a. Maybe a → H.HalogenM s f g p o m a
mustBeMounted (Just x) = pure x
mustBeMounted _ = halt "children must be mounted"


roundFractionalNum :: Number -> Number
roundFractionalNum n = roundNum (n * scalar) / scalar
  where
  scalar = 100.0

roundNum :: Number -> Number
roundNum = round
