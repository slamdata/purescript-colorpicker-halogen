module ColorPicker.Halogen.Component where

import Prelude

import CSS as CSS
import Color (Color)
import Color as Color
import ColorPicker.Halogen.Utils.Drag as Drag
import Control.Monad.Aff.Class (class MonadAff)
import DOM.Classy.Event (preventDefault)
import Data.Either (Either(..), either)
import Data.Either.Nested as Either
import Data.Foldable (fold, for_)
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Int (floor, toNumber)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Debug.Trace (spy)
import Halogen (liftEff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (halt)
import Math (pow, round)
import NumberInput.Halogen.Component as Num
import NumberInput.Range (Range(..))
import PatternInput.Halogen.Component as PatternInput


type State =
  { colorCurrent ∷ Color
  , colorNext ∷ Color
  , props ∷ Props
  }

data Message = NextChange Color | NotifyChange Color

componentHue ∷ Array ColorComponent
componentHue = [Hue]

componentSL ∷ Array ColorComponent
componentSL = [HSL_S, HSL_L]

componentSV ∷ Array ColorComponent
componentSV = [HSV_S, HSV_V]

componentRGB ∷ Array ColorComponent
componentRGB = [Red, Green, Blue]

componentHEX ∷ Array ColorComponent
componentHEX = [HEX]

type ColorComponentGroups = Array ColorComponents
type ColorComponents = Array ColorComponent

classesFor :: Props → ClassGroup → Array HH.ClassName
classesFor {classes} key = fromMaybe [] $ lookup key classes

data ClassGroup
  = Root
  | Dragger
  | Field
  | FieldGradient
  | FieldSelector
  | Slider
  | SliderSelector
  | Aside
  | Stage
  | ColorBlockCurrent
  | ColorBlockPrevious
  | Editing
  | EditingItem
  | Input
  | InputLabel
  | InputElem
  | InputElemInvalid
  | Actions
  | ActionSet

derive instance classGroupEq ∷ Eq ClassGroup
derive instance classGroupOrd ∷ Ord ClassGroup

type Props =
  { classes ∷ Map ClassGroup (Array HH.ClassName)
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
  | ComponentUpdate (Color → Maybe Color) a
  | Commit a
  | Init a

type ChildQuery = Coproduct.Coproduct2 (Num.Query Number) (PatternInput.Query Color)
type Slot = Either.Either2 ColorComponent Unit

cpColorComponent ∷ CP.ChildPath (Num.Query Number) ChildQuery ColorComponent Slot
cpColorComponent = CP.cp1

cpColorComponentHex ∷ CP.ChildPath (PatternInput.Query Color) ChildQuery Unit Slot
cpColorComponentHex = CP.cp2

type HTML m = H.ParentHTML Query ChildQuery Slot m

type DSL m = H.ParentDSL State Query ChildQuery Slot Message m

type PickerEffects r = Drag.DragEffects r

initialColor ∷ Color
initialColor = Color.hsl 0.0 0.0 0.0

picker ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ H.Component HH.HTML Query Props Message m
picker = H.lifecycleParentComponent
  { initialState: { colorCurrent: initialColor, colorNext: initialColor, props: _ }
  , render
  , eval
  , receiver: HE.input SetProps
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }

render ∷ ∀ m. State → HTML m
render { colorCurrent, colorNext, props} =
  HH.div
    [ HP.classes $ props `classesFor` Root ]
    [ dragger, aside ]
  where
  textColor c = CSS.color if Color.isLight c then CSS.black else CSS.white
  hsv = Color.toHSVA $ colorNext
  dragger =
    HH.div
      [ HP.classes $ props `classesFor` Dragger
      , HCSS.style $ textColor colorNext
      ]
      [ field, slider ]

  field =
    HH.div
      [ HP.classes $ props `classesFor` Field
      , HCSS.style $ CSS.backgroundColor $ Color.hsl hsv.h 1.0 0.5
      , HE.onMouseDown $ HE.input (Left >>> FieldDragStart)
      , HE.onTouchStart $ HE.input (Right >>> FieldDragStart)
      ]
      [ HH.div [ HP.classes $ props `classesFor` FieldGradient] []
      , HH.div
        [ HP.classes $ props `classesFor` FieldSelector
        , HCSS.style do
            CSS.display CSS.block
            CSS.left $ CSS.pct (hsv.s * 100.0)
            CSS.bottom $ CSS.pct (hsv.v * 100.0)
            CSS.backgroundColor colorNext
        ]
        []
      ]

  slider =
    HH.div
      [ HP.classes $ props `classesFor` Slider
      , HE.onMouseDown $ HE.input (Left >>> SliderDragStart)
      , HE.onTouchStart $ HE.input (Right >>> SliderDragStart)
      ]
      [ HH.div
        [ HP.classes $ props `classesFor` SliderSelector
        , HCSS.style $ CSS.top $ CSS.pct ((1.0 - hsv.h / 360.0) * 100.0)
        ]
        []
      ]

  aside =
    HH.div
      [ HP.classes $ props `classesFor` Aside ]
      [stage, editing, actions]
  stage =
    HH.div
      [ HP.classes $ props `classesFor` Stage ]
      [ HH.div
          [ HP.classes $ props `classesFor` ColorBlockCurrent
          , HCSS.style do
              CSS.backgroundColor colorNext
              textColor colorNext
          ]
          []
      , HH.div
          [ HP.tabIndex 0
          , HP.classes $ props `classesFor` ColorBlockPrevious
          , HE.onClick $ HE.input (\_ → ComponentUpdate $ const $ Just colorCurrent)
          , HCSS.style do
              CSS.backgroundColor colorCurrent
              textColor colorCurrent
          ]
          []
      ]

  editing =
    HH.div
      [ HP.classes $ props `classesFor` Editing ]
      (renderEditingItem props <$> props.editing )
  actions =
    HH.div
      [ HP.classes $ props `classesFor` Actions ]
      [ HH.button
          [ HP.classes $ props `classesFor` ActionSet
          , HE.onClick $ HE.input \_ -> Commit
          ]
          [ HH.text "Set" ]
      ]

renderEditingItem ∷ ∀ m. Props → ColorComponents  → HTML m
renderEditingItem props x = HH.div [ HP.classes $ props `classesFor` EditingItem ] $ x <#> case _ of
  Hue   → embedNum hasValRound Hue   $ mkConf props confHue
  HSV_S → embedNum hasValRound HSV_S $ mkConf props confSaturation
  HSV_V → embedNum hasValRound HSV_V $ mkConf props confValue
  HSL_S → embedNum hasValRound HSL_S $ mkConf props confSaturation
  HSL_L → embedNum hasValRound HSL_L $ mkConf props confLightness
  Red   → embedNum hasvalCail  Red   $ mkConf props confRed
  Green → embedNum hasvalCail  Green $ mkConf props confGreen
  Blue  → embedNum hasvalCail  Blue  $ mkConf props confBlue
  HEX   → renderHex

  where
  hasValRound ∷ Num.HasNumberInputVal Number
  hasValRound = Num.numberHasNumberInputVal
    {fromString = Num.numberHasNumberInputVal.fromString >>> map roundFractionalNum}

  hasvalCail ∷ Num.HasNumberInputVal Number
  hasvalCail = Num.numberHasNumberInputVal
    {fromString = Num.numberHasNumberInputVal.fromString >>> map roundNum}

  embedNum ∷ Num.HasNumberInputVal Number → ColorComponent → Num.Config Number → HTML m
  embedNum hasVal component conf = input conf.placeholder
    $ HH.slot' cpColorComponent component (Num.input hasVal conf) unit
    $ HE.input \(Num.NotifyChange val) → ComponentUpdate $ \color →
        val <#> \n → do
          case component of
            Hue   → modifyHSL (_{h = n}) color
            HSL_S → modifyHSL (_{s = n / 100.0}) color
            HSL_L → modifyHSL (_{l = n / 100.0}) color
            HSV_S → modifyHSV (_{s = n / 100.0}) color
            HSV_V → modifyHSV (_{v = n / 100.0}) color
            Red   → modifyRGB (_{r = asInt n}) color
            Green → modifyRGB (_{g = asInt n}) color
            Blue  → modifyRGB (_{b = asInt n}) color
            HEX   → color -- hex values will not be comming from Number input
    where
    asInt = floor
  input ∷ String → HTML m → HTML m
  input label child =
    HH.label [HP.classes $ props `classesFor` Input]
      [ HH.span [HP.classes $ props `classesFor` InputLabel] [HH.text label]
      , child
      ]
  renderHex = input "#"
    $ HH.slot' cpColorComponentHex unit (PatternInput.input
      { fromString: \str → Color.fromHexString $ "#" <> str
      , toString: \color → String.toUpper $ String.drop 1 $ Color.toHexString color
      }
      { title: "Hex"
      , placeholder: "HEX"
      , root: props `classesFor` InputElem
      , rootInvalid: props `classesFor` InputElemInvalid
      }) unit
    $ HE.input \(PatternInput.NotifyChange val) → ComponentUpdate $ const val

type PreNumConf a = { title ∷ String, placeholder ∷ String, range ∷ Range a }

mkConf ∷ ∀ a. Props → PreNumConf a → Num.Config a
mkConf props { title, placeholder, range } =
  { title
  , placeholder
  , range
  , root: props `classesFor` InputElem
  , rootInvalid: props `classesFor` InputElemInvalid
  , rootLength: const []
  }

confRed ∷ PreNumConf Number
confRed =
  { title: "Red"
  , placeholder: "R"
  , range: MinMax 0.0 256.0
  }

confGreen ∷ PreNumConf Number
confGreen =
  { title: "Green"
  , placeholder: "G"
  , range: MinMax 0.0 256.0
  }

confBlue ∷ PreNumConf Number
confBlue =
  { title: "Blue"
  , placeholder: "B"
  , range: MinMax 0.0 256.0
  }

confHue ∷ PreNumConf Number
confHue =
  { title: "Hue"
  , placeholder: "H"
  , range: MinMax 0.0 360.0
  }
confSaturation ∷ PreNumConf Number
confSaturation =
  { title: "Saturation"
  , placeholder: "S"
  , range: MinMax 0.0 100.0
  }
confLightness ∷ PreNumConf Number
confLightness =
  { title: "Lightness"
  , placeholder: "L"
  , range: MinMax 0.0 100.0
  }

confValue ∷ PreNumConf Number
confValue =
  { title: "Value"
  , placeholder: "V"
  , range: MinMax 0.0 100.0
  }


updateColor :: ∀ m. State → Color → DSL m Unit
updateColor state colorNext = do
  H.put state{colorNext = colorNext}
  H.raise $ NextChange colorNext
  propagate


eval ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ Query ~> DSL m
eval = case _ of
  Init next → do
    propagate
    pure next
  Commit next → do
    state ← H.get
    H.put $ state{colorCurrent = state.colorNext, colorNext = state.colorNext}
    void $ map spy H.get
    H.raise $ NotifyChange state.colorNext
    pure next
  ComponentUpdate update next → do
    state ← H.get
    for_ (update state.colorNext) $ \color' → do
      updateColor state $ color'
    pure next
  SetProps props next → do
    H.modify _{props = props}
    pure next
  FieldDragMove drag next → do
    case drag of
      Drag.Move event dragData → do
        state ← H.get
        let
          color = state.colorNext
          hsv = Color.toHSVA color
          s = roundFractionalNum' 2 $ (dragData.progress.x)
          v = roundFractionalNum' 2 $ (1.0 - dragData.progress.y)
        updateColor state (Color.hsv hsv.h s v)
        pure unit
      Drag.Done event → pure unit
    pure next
  SliderDragMove drag next → do
    case drag of
      Drag.Move event dragData → do
        state ← H.get
        let
          h = roundFractionalNum $ (1.0 - dragData.progress.y) * 360.0
          hsl = Color.toHSLA $ state.colorNext
        updateColor state (Color.hsl h hsl.s hsl.l)
      Drag.Done event → pure unit
    pure next
  SliderDragStart event next → startDrag SliderDragMove event next
  FieldDragStart event next → startDrag FieldDragMove event next
  where
  startDrag
    ∷ ∀ a
    . (∀ b. Drag.DragEvent → b → Query b)
    → Drag.CursorEvent
    → a
    → DSL m a
  startDrag action event next = do
    H.subscribe $ Drag.dragEventSource event \drag →
      Just (action drag H.Listening)
    liftEff $ either preventDefault preventDefault event
    initialDragData ← liftEff $ Drag.mkFirstDragData event
    eval $ action (Drag.Move event initialDragData) next


propagate ∷ ∀ m. DSL m Unit
propagate = do
  { colorNext, props: { editing }} ← H.get
  let
    c = colorNext
    hsl = Color.toHSLA c
    hsv = Color.toHSVA c
    rgb = Color.toRGBA c
  for_ (fold editing) $ case _ of
    Hue   → H.query' cpColorComponent Hue   (set $ roundFractionalNum hsl.h) >>= mustBeMounted
    HSL_S → H.query' cpColorComponent HSL_S (set $ roundFractionalNum $ 100.0 * hsl.s) >>= mustBeMounted
    HSL_L → H.query' cpColorComponent HSL_L (set $ roundFractionalNum $ 100.0 * hsl.l) >>= mustBeMounted
    HSV_S → H.query' cpColorComponent HSV_S (set $ roundFractionalNum $ 100.0 * hsv.s) >>= mustBeMounted
    HSV_V → H.query' cpColorComponent HSV_V (set $ roundFractionalNum $ 100.0 * hsv.v) >>= mustBeMounted
    Red   → H.query' cpColorComponent Red   (set $ roundNum $ toNumber rgb.r) >>= mustBeMounted
    Green → H.query' cpColorComponent Green (set $ roundNum $ toNumber rgb.g) >>= mustBeMounted
    Blue  → H.query' cpColorComponent Blue  (set $ roundNum $ toNumber rgb.b) >>= mustBeMounted
    HEX   → H.query' cpColorComponentHex unit (H.action $ PatternInput.SetValue $ Just $ c) >>= mustBeMounted
  where
  set n = H.action (Num.SetValue $ Just $ roundFractionalNum n)

type RecordHSLA = { h ∷ Number, s ∷ Number, l ∷ Number, a ∷ Number }
type RecordHSVA = { h ∷ Number, s ∷ Number, v ∷ Number, a ∷ Number }
type RecordRGBA = { r ∷ Int, g ∷ Int, b ∷ Int, a ∷ Number }

modifyHSL ∷ (RecordHSLA → RecordHSLA) → Color → Color
modifyHSL f c = case f (Color.toHSLA c) of {h, s, l, a} → Color.hsla h s l a

modifyHSV ∷ (RecordHSVA → RecordHSVA) → Color → Color
modifyHSV f c = case f (Color.toHSVA c) of {h, s, v, a} → Color.hsva h s v a

modifyRGB ∷ (RecordRGBA → RecordRGBA) → Color → Color
modifyRGB f c = case f (Color.toRGBA c) of {r, g, b, a} → Color.rgba r g b a

mustBeMounted ∷ ∀ s f g p o m a. Maybe a → H.HalogenM s f g p o m a
mustBeMounted (Just x) = pure x
mustBeMounted _ = halt "children must be mounted"


roundFractionalNum ∷ Number → Number
roundFractionalNum = roundFractionalNum' 0

roundFractionalNum' ∷ Int → Number → Number
roundFractionalNum' digits n = roundNum (n * scalar) / scalar
  where
  scalar = pow 10.0 (toNumber $ digits + 2)

roundNum ∷ Number → Number
roundNum = round
