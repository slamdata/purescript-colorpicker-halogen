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
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Debug.Trace (spy)
import Halogen (liftEff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (halt)
import NumberInput.Halogen.Component as Num
import ColorPicker.Halogen.ColorComponents (ColorComponent(..), PreNumConf, PreTextConf)
import PatternInput.Halogen.Component as PatternInput


type State =
  { colorCurrent ∷ Color
  , colorNext ∷ Color
  , props ∷ Props
  }

data Message = NextChange Color | NotifyChange Color


type ColorComponentGroups = Array ColorComponents
type ColorComponents = Array ColorComponent

classesFor ∷ Props → ClassGroup → Array HH.ClassName
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
  | ColorBlockNext
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
type Slot = Either.Either2 String String

cpNumComponent ∷ CP.ChildPath (Num.Query Number) ChildQuery String Slot
cpNumComponent = CP.cp1

cpTextComponent ∷ CP.ChildPath (PatternInput.Query Color) ChildQuery String Slot
cpTextComponent = CP.cp2

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
          [ HP.classes $ props `classesFor` ColorBlockNext
          , HP.title "Next value"
          , HCSS.style do
              CSS.backgroundColor colorNext
              textColor colorNext
          ]
          []
      , HH.div
          [ HP.tabIndex 0
          , HP.classes $ props `classesFor` ColorBlockCurrent
          , HP.title "Current value"
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
          , HE.onClick $ HE.input \_ → Commit
          ]
          [ HH.text "Set" ]
      ]

renderEditingItem ∷ ∀ m. Props → ColorComponents → HTML m
renderEditingItem props x = HH.div [ HP.classes $ props `classesFor` EditingItem ] $ x <#> renderItem
  where
  renderItem = case _ of
    NumberComponentSpec { key, hasNumVal, update, config } →
      input config.prefix
        $ HH.slot' cpNumComponent key (Num.input hasNumVal (mkNumConf props config)) unit
        $ HE.input \(Num.NotifyChange val) → ComponentUpdate $ \color → update <$> val >>= (_ $ color)
    TextComponentSpec { hasInputVal, key, config } →
      input config.prefix
        $ HH.slot' cpTextComponent key (PatternInput.input hasInputVal (mkTextConfig props config)) unit
        $ HE.input \(PatternInput.NotifyChange val) → ComponentUpdate $ const val

  input ∷ String → HTML m → HTML m
  input label child =
    HH.label [HP.classes $ props `classesFor` Input]
      [ HH.span [HP.classes $ props `classesFor` InputLabel] [HH.text label]
      , child
      ]

mkTextConfig ∷ ∀ a. Props → PreTextConf → PatternInput.Config a
mkTextConfig props { title, placeholder } =
  { title
  , placeholder
  , root: props `classesFor` InputElem
  , rootInvalid: props `classesFor` InputElemInvalid
  }

mkNumConf ∷ ∀ a. Props → PreNumConf a → Num.Config a
mkNumConf props { title, placeholder, range } =
  { title
  , placeholder
  , range
  , root: props `classesFor` InputElem
  , rootInvalid: props `classesFor` InputElemInvalid
  , rootLength: const []
  }

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
          s = dragData.progress.x
          v = 1.0 - dragData.progress.y
        updateColor state (Color.hsv hsv.h s v)
        pure unit
      Drag.Done event → pure unit
    pure next
  SliderDragMove drag next → do
    case drag of
      Drag.Move event dragData → do
        state ← H.get
        let
          h = (1.0 - dragData.progress.y) * 360.0
          hsl = Color.toHSLA $ state.colorNext
        updateColor state (Color.hsl h hsl.s hsl.l)
      Drag.Done event → pure unit
    pure next
  SliderDragStart event next → startDrag SliderDragMove event next
  FieldDragStart event next → startDrag FieldDragMove event next

startDrag
  ∷ ∀ m r a
  . MonadAff (PickerEffects r) m
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

updateColor ∷ ∀ m. State → Color → DSL m Unit
updateColor state colorNext = do
  H.put state{colorNext = colorNext}
  H.raise $ NextChange colorNext
  propagate

propagate ∷ ∀ m. DSL m Unit
propagate = do
  { colorNext, props: { editing }} ← H.get
  let
    colorEnv =
      { hsl: Color.toHSLA colorNext
      , hsv: Color.toHSVA colorNext
      , rgb: Color.toRGBA colorNext
      }
  for_ (fold editing) $  \spec → mustBeMounted =<< case spec of
    TextComponentSpec { key } →
      H.query' cpTextComponent key (H.action $ PatternInput.SetValue $ Just colorNext)
    NumberComponentSpec {key, read} →
      H.query' cpNumComponent key (H.action $ Num.SetValue $ Just $ read colorEnv)

mustBeMounted ∷ ∀ s f g p o m a. Maybe a → H.HalogenM s f g p o m a
mustBeMounted (Just x) = pure x
mustBeMounted _ = halt "children must be mounted"
