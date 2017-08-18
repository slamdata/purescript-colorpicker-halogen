module ColorPicker.Halogen.Component
  ( picker
  , Query(GetValue, SetValue, Commit)
  , ValueHistory
  , Message(..)
  , Props
  , ClassGroup(..)
  , PickerEffects
  )
  where

import Prelude

import CSS as CSS
import Color (Color)
import Color as Color
import ColorPicker.Halogen.ColorComponents (ColorComponent(..), DragComponentView(..), InputTextValue, LazyColor, NumberComponentView(..), PositionUpdate, TextComponentView(..), mkLazyColor, runExistsRow)
import ColorPicker.Halogen.Layout as L
import ColorPicker.Halogen.Utils.Drag as Drag
import Control.Monad.Aff.Class (class MonadAff)
import DOM.Classy.Event (preventDefault)
import Data.Array (head, index, mapWithIndex, nubBy, take)
import Data.Either (Either(..), either)
import Data.Either.Nested as Either
import Data.Foldable (foldr, for_)
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

type ValueHistory a =  { old ∷ Array a, current ∷ a }
type State =
  { color ∷ ValueHistory LazyColor
  , inputValues ∷ Map Cursor InputTextValue
  , props ∷ Props
  }

data Message = NextChange Color | NotifyChange Color

classesFor ∷ Props → ClassGroup → Array HH.ClassName
classesFor {classes} key = fromMaybe [] $ lookup key classes

data ClassGroup
  = Stage
  | ColorBlockCurrent
  | ColorBlockOld
  | Actions
  | ActionSet

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
  | TextComponentBlur Cursor a
  | Commit a
  | Init a
  | GetValue (ValueHistory LazyColor → a) -- TODO make ValueHistory and use normal Color here
  | SetValue (ValueHistory LazyColor) a

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
  { initialState:
      { color: { current: mkLazyColor initialColor, old: [] }
      , inputValues: mempty
      , props: _ }
  , render
  , eval
  , receiver: HE.input SetProps
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }

render ∷ ∀ m. State → HTML m
render state = renderLayout state List.Nil state.props.layout


renderLayout ∷ ∀ m. State → Cursor → L.Layout → HTML m
renderLayout state@{ color, inputValues, props} cursor = case _ of
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
    let
      current = HH.div
        [ HP.classes $ classesFor props ColorBlockCurrent
        , HP.title "Current value"
        , HCSS.style $ CSS.backgroundColor color.current.color
        ] []

      old = take 4 color.old <#> \c ->
        HH.div
          [ HP.tabIndex 0
          , HP.classes $ classesFor props ColorBlockOld
          , HP.title "Old value"
          , HE.onClick $ HE.input (\_ → ComponentUpdate $ const $ Just c.color)
          , HCSS.style $ CSS.backgroundColor c.color
          ] []
    in
      HH.div
        [ HP.classes $ props `classesFor` Stage ] $
        [ current ] <> old
  L.Component c →  case c of
    DragComponentSpec spec →
      let
        run :: ∀ r. DragComponentView r -> HTML m
        run (DragComponentView view) = view color.current $
          [ HE.onMouseDown $ HE.input (Left >>> DragStart spec.update)
          , HE.onTouchStart $ HE.input (Right >>> DragStart spec.update)
          ]
      in runExistsRow run spec.view
    NumberComponentSpec spec →
      let
        NumberComponentView view = spec.view
      in
        view color.current $
          HH.slot'
            cpNumComponent
            cursor
            Num.input
            spec.props
            (HE.input \(Num.NotifyChange val) → NumberComponentUpdate cursor val)
    TextComponentSpec spec →
      let
        run :: ∀ r. TextComponentView r -> HTML m
        run (TextComponentView view) = view color.current
          (lookup cursor inputValues)
          [ HE.onValueInput $ HE.input $ TextComponentUpdate cursor spec.fromString
          , HE.onBlur $ HE.input_ $ TextComponentBlur cursor
          ]
      in runExistsRow run spec.view


eval ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ Query ~> DSL m
eval = case _ of
  TextComponentBlur cursor next → do
    state ← H.get
    let val = lookup cursor state.inputValues
    for_ (lookup cursor state.inputValues) \val ->
      when val.isValid $ H.modify _ { inputValues = mempty :: Map Cursor InputTextValue }
    pure next
  TextComponentUpdate cursor fromString str next → do
    let
      color = fromString str
      isValid = case color of
        Nothing → false
        Just _ → true
    H.modify \s → s { inputValues = insert cursor {isValid, value: str} s.inputValues }
    eval $ ComponentUpdate (const color) next
  NumberComponentUpdate cursor num next → do
    { color, props } ← H.get
    case focus cursor props.layout of
      Just (L.Component (NumberComponentSpec { update })) → eval $
       ComponentUpdate
        (\_ → update <$> num >>= (_ $ color.current))
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
    -- TODO disable Commit button if this condition is not true
    when (Just state.color.current.color /= (map _.color $ head state.color.old)) do
      H.put $ state{ color =
        { current: state.color.current
        , old: nubBy (\a b -> eq a.color b.color) $ [state.color.current] <> state.color.old
        }}
      H.raise $ NotifyChange state.color.current.color
    pure next
  ComponentUpdate update next → do
    state ← H.get
    for_ (update state.color.current.color) $ \color' → do
      updateColor state $ color'
    pure next
  SetProps props next → do
    H.modify _{props = props}
    pure next
  DragMove update drag next → do
    case drag of
      Drag.Move event dragData → do
        state ← H.get
        updateColor state $ update dragData.progress state.color.current
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
  H.put state{ color { current = mkLazyColor colorNext } }
  H.raise $ NextChange colorNext
  propagate


propagate ∷ ∀ m. DSL m Unit
propagate = do
  { color, props: { layout }} ← H.get
  propagateLayout
    color.current
    List.Nil
    layout
  where
  propagateLayout ∷ LazyColor → Cursor → L.Layout → DSL m Unit
  propagateLayout color cursor = case _ of
    L.Group _ l → void $ sequence $ mapWithIndex (\idx → propagateLayout color (List.Cons idx cursor)) l
    L.Stage → pure unit
    L.Actions → pure unit
    L.Component c → case c of
      DragComponentSpec _ → pure unit
      TextComponentSpec _ → pure unit
      NumberComponentSpec { read } →
        H.query' cpNumComponent cursor (H.action $ Num.SetValue $ Just $ read color) >>= mustBeMounted

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
