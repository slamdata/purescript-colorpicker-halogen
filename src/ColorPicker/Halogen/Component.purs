module ColorPicker.Halogen.Component
  ( picker
  , Query(GetValue, SetValue, Commit)
  , ValueProgress
  , Message(..)
  , Props
  , ClassGroup(..)
  , ColorComponentGroups
  , ColorComponents
  , PickerEffects
  )
  where

import Prelude

import CSS as CSS
import Color (Color)
import Color as Color
import ColorPicker.Halogen.ColorComponents (ColorComponent(..), PreNumConf)
import ColorPicker.Halogen.Utils.Drag as Drag
import Control.Monad.Aff.Class (class MonadAff)
import Control.MonadZero (guard)
import DOM.Classy.Event (preventDefault)
import Data.Either (Either(..), either, isLeft)
import Data.Either.Nested as Either
import Data.Foldable (fold, foldMap, for_)
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Halogen (liftEff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (halt)
import NumberInput.Halogen.Component as Num


type ValueProgress a =  { current ∷ a, next ∷ a }
type State =
  { color ∷ ValueProgress Color
  , inputs ∷ Map String (Either String String)
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
  | IsLight
  | IsDark

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
  | GetValue (ValueProgress Color → a)
  | SetValue (ValueProgress Color) a
  | UpdateTextInput String String (Maybe Color) a

type ChildQuery = Coproduct.Coproduct1 (Num.Query Number)
type Slot = Either.Either1 String

cpNumComponent ∷ CP.ChildPath (Num.Query Number) ChildQuery String Slot
cpNumComponent = CP.cp1


type HTML m = H.ParentHTML Query ChildQuery Slot m

type DSL m = H.ParentDSL State Query ChildQuery Slot Message m

type PickerEffects r = Drag.DragEffects r

initialColor ∷ Color
initialColor = Color.hsl 0.0 0.0 0.0

picker ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ H.Component HH.HTML Query Props Message m
picker = H.lifecycleParentComponent
  { initialState: { color: { current: initialColor, next: initialColor }, inputs: mempty, props: _ }
  , render
  , eval
  , receiver: HE.input SetProps
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }

render ∷ ∀ m. State → HTML m
render state@{ color, inputs, props} =
  HH.div
    [ HP.classes $ props `classesFor` Root ]
    [ dragger, aside ]
  where
  colorClasses c = props `classesFor` (if Color.isLight c then IsLight else IsDark)
  hsv = Color.toHSVA $ color.next

  dragger =
    HH.div
      [ HP.classes $ (props `classesFor` Dragger) <> colorClasses color.next ]
      [ field, slider ]

  field =
    HH.div
      [ HP.classes $ props `classesFor` Field
      , HCSS.style $ CSS.backgroundColor $ Color.hsl hsv.h 1.0 0.5
      , HE.onMouseDown $ HE.input (Left >>> FieldDragStart)
      , HE.onTouchStart $ HE.input (Right >>> FieldDragStart)
      ]
      [ HH.div
        [ HP.classes $ props `classesFor` FieldSelector
        , HCSS.style do
            CSS.display CSS.block
            CSS.left $ CSS.pct (hsv.s * 100.0)
            CSS.bottom $ CSS.pct (hsv.v * 100.0)
            CSS.backgroundColor color.next
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
          [ HP.classes $ (props `classesFor` ColorBlockNext) <> colorClasses color.next
          , HP.title "Next value"
          , HCSS.style $ CSS.backgroundColor color.next
          ]
          []
      , HH.div
          [ HP.tabIndex 0
          , HP.classes $ (props `classesFor` ColorBlockCurrent) <> colorClasses color.current
          , HP.title "Current value"
          , HE.onClick $ HE.input (\_ → ComponentUpdate $ const $ Just color.current)
          , HCSS.style $ CSS.backgroundColor color.current
          ]
          []
      ]

  editing =
    HH.div
      [ HP.classes $ props `classesFor` Editing ]
      (renderEditingItem state <$> props.editing )

  actions =
    HH.div
      [ HP.classes $ props `classesFor` Actions ]
      [ HH.button
          [ HP.classes $ props `classesFor` ActionSet
          , HE.onClick $ HE.input \_ → Commit
          ]
          [ HH.text "Set" ]
      ]

renderEditingItem ∷ ∀ m. State -> ColorComponents → HTML m
renderEditingItem { inputs, props } x = HH.div [ HP.classes $ props `classesFor` EditingItem ] $ x <#> renderItem
  where
  renderItem = case _ of
    NumberComponentSpec { key, hasNumVal, update, config } →
      input config.prefix
        $ pure
        $ HH.slot' cpNumComponent key (Num.input hasNumVal (mkNumConf props config)) unit
        $ HE.input \(Num.NotifyChange val) → ComponentUpdate $ \color → update <$> val >>= (_ $ color)
    TextComponentSpec { fromString, key, config } →
      input config.prefix $ flip foldMap (lookup key inputs) \val -> pure $ HH.input
        [ HP.type_ HP.InputText
        , HP.classes
          $  (props `classesFor` InputElem)
          <> (guard (isLeft val) *> (props `classesFor` InputElemInvalid))
        , HP.title config.title
        , HP.placeholder config.placeholder
        , HP.value $ either id id val
        , HE.onValueInput $ HE.input $ \ str -> UpdateTextInput key str (fromString str)
        ]

  input ∷ String → Array (HTML m) → HTML m
  input label child =
    HH.label [HP.classes $ props `classesFor` Input] $
      [ HH.span [HP.classes $ props `classesFor` InputLabel] [HH.text label]] <> child

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
  UpdateTextInput key str color next → do
    let
      val = case color of
        Nothing -> Left str
        Just _ -> Right str
    H.modify \s -> s { inputs = insert key val s.inputs }
    eval $ ComponentUpdate (const color) next
  SetValue val next → do
    state ← H.get
    H.put $ state{ color = val }
    pure next
  GetValue next → H.get <#> (_.color >>> next)
  Init next → do
    propagate
    pure next
  Commit next → do
    state ← H.get
    H.put $ state{ color {current = state.color.next, next = state.color.next} }
    H.raise $ NotifyChange state.color.next
    pure next
  ComponentUpdate update next → do
    state ← H.get
    for_ (update state.color.next) $ \color' → do
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
          color = state.color.next
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
          hsl = Color.toHSLA $ state.color.next
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
  H.put state{ color { next = colorNext } }
  H.raise $ NextChange colorNext
  propagate

propagate ∷ ∀ m. DSL m Unit
propagate = do
  { color, props: { editing }} ← H.get
  let
    colorEnv =
      { hsl: Color.toHSLA color.next
      , hsv: Color.toHSVA color.next
      , rgb: Color.toRGBA color.next
      }
  for_ (fold editing) $  \spec → case spec of
    TextComponentSpec { key, toString } →
      H.modify \s -> s { inputs = insert key (Right $ toString color.next) s.inputs }
    NumberComponentSpec {key, read} →
      H.query' cpNumComponent key (H.action $ Num.SetValue $ Just $ read colorEnv) >>= mustBeMounted

mustBeMounted ∷ ∀ s f g p o m a. Maybe a → H.HalogenM s f g p o m a
mustBeMounted (Just x) = pure x
mustBeMounted _ = halt "children must be mounted"
