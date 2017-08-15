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
import ColorPicker.Halogen.ColorComponents (ColorComponent(..), ColorEnv, PositionUpdate, mapInputProps, DragComponentView(..), DragComponentViewX, unDragComponentViewX)
import ColorPicker.Halogen.Layout as L
import ColorPicker.Halogen.Utils.Drag as Drag
import Control.Monad.Aff.Class (class MonadAff)
import Control.MonadZero (guard)
import DOM.Classy.Event (preventDefault)
import DOM.Event.Types (MouseEvent, TouchEvent)
import Data.Array (index, mapWithIndex)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either, isLeft)
import Data.Either.Nested as Either
import Data.Foldable (foldMap, foldr, for_)
import Data.Functor.Coproduct.Nested as Coproduct
import Data.List as List
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Traversable (sequence)
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
  , inputs ∷ Map Cursor (Either String String) -- Left is invalid, Right is valid
  , props ∷ Props
  }

data Message = NextChange Color | NotifyChange Color


type ColorComponentGroups = Array ColorComponents
type ColorComponents = Array ColorComponent

classesFor ∷ Props → ClassGroup → Array HH.ClassName
classesFor {classes} key = fromMaybe [] $ lookup key classes

data ClassGroup
  = Stage
  | ColorBlockCurrent
  | ColorBlockNext
  | Actions
  | ActionSet
  | IsLight
  | IsDark

derive instance classGroupEq ∷ Eq ClassGroup
derive instance classGroupOrd ∷ Ord ClassGroup

type Props =
  { classes ∷ Map ClassGroup (Array HH.ClassName)
  , layout ∷ L.Layout
  }


data Query a
  = SetProps Props a
  | DragStart PositionUpdate Drag.CursorEvent a
  | DragMove PositionUpdate Drag.DragEvent a
  -- TODO `ComponentUpdate` could be removed at some point
  | ComponentUpdate (Color → Maybe Color) a
  | NumberComponentUpdate Cursor (Maybe Number) a
  | TextComponentUpdate Cursor (String → Maybe Color) String a
  | Commit a
  | Init a
  | GetValue (ValueProgress Color → a)
  | SetValue (ValueProgress Color) a

type ChildQuery = Coproduct.Coproduct1 (Num.Query Number)
type Slot = Either.Either1 Cursor

cpNumComponent ∷ CP.ChildPath (Num.Query Number) ChildQuery Cursor Slot
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
render state@{ color, inputs, props} = renderLayout state List.Nil props.layout


renderLayout ∷ ∀ m. State → Cursor → L.Layout → HTML m
renderLayout state@{ color, inputs, props} cursor = case _ of
  L.Group classes l →
    HH.div
      [ HP.classes classes ]
      $ mapWithIndex (\idx → renderLayout state (List.Cons idx cursor)) l
  L.Actions →
    HH.div
      [ HP.classes $ props `classesFor` Actions ]
      [ HH.button
          [ HP.classes $ props `classesFor` ActionSet
          , HE.onClick $ HE.input \_ → Commit
          ]
          [ HH.text "Set" ]
      ]
  L.Stage →
    HH.div
      [ HP.classes $ props `classesFor` Stage ]
      [ HH.div
          [ HP.classes $ classesFor props ColorBlockNext <> colorClasses props color.next
          , HP.title "Next value"
          , HCSS.style $ CSS.backgroundColor color.next
          ]
          []
      , HH.div
          [ HP.tabIndex 0
          , HP.classes $ classesFor props ColorBlockCurrent <> colorClasses props color.current
          , HP.title "Current value"
          , HE.onClick $ HE.input (\_ → ComponentUpdate $ const $ Just color.current)
          , HCSS.style $ CSS.backgroundColor color.current
          ]
          []
      ]
  L.Component c → renderColorComponent state cursor c

  where
  hsv = Color.toHSVA $ color.next
  colorEnv = calcColorEnv color.next
  renderColorComponent ∷ State → Cursor → ColorComponent → HTML m
  renderColorComponent { inputs, props } cursor = case _ of
    -- TODO refactor so spec is just a function of type
    -- ∷ ∀ p r i
    -- . ColorEnv
    -- → IProp (onMouseDown :: MouseEvent, onTouchStart :: TouchEvent | r) i
    -- → Array (HTML p i)
    --
    -- we can make other specs in the same fession instead
    --   off caryng around multipe env -> style functions
    DragComponentSpec spec → renderDrag colorEnv spec
    NumberComponentSpec { styles, hasNumVal, update, config } → let computedStyles = mapInputProps (_ $ colorEnv) styles in
      input computedStyles config.prefix
        $ pure
        $ HH.slot'
          cpNumComponent
          cursor
          Num.input
          { title: config.title
          , hasNumberValue: hasNumVal
          , placeholder: config.placeholder
          , range: config.range
          , root: computedStyles.elem.classes
          , rootInvalid: computedStyles.elemInvalid.classes
          , rootLength: const []
          }
        $ HE.input \(Num.NotifyChange val) → NumberComponentUpdate cursor val
    TextComponentSpec { styles, fromString, config } → let computedStyles = mapInputProps (_ $ colorEnv) styles in
      input computedStyles config.prefix
        $ flip foldMap (lookup cursor inputs) \val → pure
        $ HH.input
        [ HP.type_ HP.InputText
        , HP.classes
          $  computedStyles.elem.classes
          <> (guard (isLeft val) *> (computedStyles.elemInvalid.classes))
        , HP.title config.title
        , HP.placeholder config.placeholder
        , HP.value $ either id id val
        , HE.onValueInput $ HE.input $ TextComponentUpdate cursor fromString
        ]
    where
    input styles label child =
      HH.label [HP.classes styles.root.classes] $
        [ HH.span [HP.classes styles.label.classes] [HH.text label]] <> child

colorClasses ∷ Props → Color → Array HH.ClassName
colorClasses props c = classesFor props $ if Color.isLight c then IsLight else IsDark


eval ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ Query ~> DSL m
eval = case _ of
  TextComponentUpdate cursor fromString str next → do
    {props} ← H.get
    let
      color = fromString str
      val = case color of
        Nothing → Left str
        Just _ → Right str
    H.modify \s → s { inputs = insert cursor val s.inputs }
    eval $ ComponentUpdate (const color) next
  NumberComponentUpdate cursor num next → do
    { color, props } ← H.get
    case focus cursor props.layout of
      Just (L.Component (NumberComponentSpec { update })) → eval $
       ComponentUpdate
        (\_ → update <$> num >>= (_ $ calcColorEnv color.next))
        next
      _ → pure next
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
  DragMove update drag next → do
    case drag of
      Drag.Move event dragData → do
        state ← H.get
        updateColor state $ update dragData.progress (calcColorEnv state.color.next)
        pure unit
      Drag.Done event → pure unit
    pure next
  DragStart update event next → do
    H.subscribe $ Drag.dragEventSource event \drag →
      Just (DragMove update drag H.Listening)
    liftEff $ either preventDefault preventDefault event
    initialDragData ← liftEff $ Drag.mkFirstDragData event
    eval $ DragMove update (Drag.Move event initialDragData) next

updateColor ∷ ∀ m. State → Color → DSL m Unit
updateColor state colorNext = do
  H.put state{ color { next = colorNext } }
  H.raise $ NextChange colorNext
  propagate


calcColorEnv ∷ Color → ColorEnv
calcColorEnv color =
  { hsl: Color.toHSLA color
  , hsv: Color.toHSVA color
  , rgb: Color.toRGBA color
  , isLight: Color.isLight color
  , color: color
  }

propagate ∷ ∀ m. DSL m Unit
propagate = do
  { color, props: { layout }} ← H.get
  propagateLayout
    (calcColorEnv color.next)
    List.Nil
    layout
  where
  propagateLayout ∷ ColorEnv → Cursor → L.Layout → DSL m Unit
  propagateLayout colorEnv cursor = case _ of
    L.Group _ l → void $ sequence $ mapWithIndex (\idx → propagateLayout colorEnv (List.Cons idx cursor)) l
    L.Stage → pure unit
    L.Actions → pure unit
    L.Component c → case c of
      DragComponentSpec _ → pure unit
      TextComponentSpec { toString } →
        H.modify \s → s { inputs = insert cursor (Right $ toString colorEnv) s.inputs }
      NumberComponentSpec { read} →
        H.query' cpNumComponent cursor (H.action $ Num.SetValue $ Just $ read colorEnv) >>= mustBeMounted

mustBeMounted ∷ ∀ s f g p o m a. Maybe a → H.HalogenM s f g p o m a
mustBeMounted (Just x) = pure x
mustBeMounted _ = halt "children must be mounted"

type Cursor = List.List Int

focus :: Cursor → L.Layout → Maybe L.Layout
focus cursor layout = foldr f (Just layout) cursor
  where
  f idx = case _ of
    Just (L.Group _ l) → index l idx
    _ → Nothing

props :: forall r.
  PositionUpdate -> Array
       (HH.IProp
          ( onMouseDown :: MouseEvent
          , onTouchStart :: TouchEvent
          | r
          )
          (Query Unit)
       )
props update =
  [ HE.onMouseDown $ HE.input (Left >>> DragStart update)
  , HE.onTouchStart $ HE.input (Right >>> DragStart update)
  ]

renderDrag
  ∷ ∀ m q
  . ColorEnv
  → { update ∷ PositionUpdate
    , view ∷ DragComponentViewX
    }
  → HTML m
renderDrag colorEnv spec = unDragComponentViewX (\(DragComponentView f) -> bimap absurd absurd $ f colorEnv []) spec.view


-- funcMM :: ∀ r i p
--   . DragComponentViewX
--   → DragComponentView r i p
-- -- funcMM view = unDragComponentViewX (\(DragComponentView x) -> DragComponentView x) view
-- funcMM view =
