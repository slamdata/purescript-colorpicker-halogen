module ColorPicker.Halogen.Component
  ( picker
  , Query(GetValue, SetValue, Commit)
  , Message(..)
  , Props
  , PickerEffects
  )
  where

import Prelude

import Color (Color)
import Color as Color
import ColorPicker.Halogen.Layout (ChildLayout(..), InputTextValue, Layout(..), PickerComponent(..), PositionUpdate, ValueHistory)
import ColorPicker.Halogen.Utils.Drag as Drag
import Control.Monad.Aff.Class (class MonadAff)
import DOM.Classy.Event (preventDefault)
import Data.Array (index, mapWithIndex, nub)
import Data.Either (Either(..), either)
import Data.Either.Nested as Either
import Data.Foldable (fold, foldr, for_)
import Data.Functor.Coproduct.Nested as Coproduct
import Data.List as List
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (mempty)
import Data.Traversable (sequence)
import Halogen (liftEff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM (halt)
import NumberInput.Halogen.Component as Num

type State =
  { color ∷ ValueHistory Color
  , inputValues ∷ Map Cursor InputTextValue
  , props ∷ Props
  }

data Message = NextChange Color | NotifyChange Color

type Props =
  { layout ∷ Layout
  }

data Query a
  = SetProps Props a
  | DragStart PositionUpdate Drag.CursorEvent a
  | DragMove PositionUpdate Drag.DragEvent a
  | UpdateCurrentColor Color a
  | NumberComponentUpdate Cursor (Maybe Number) a
  | TextComponentUpdate Cursor (String → Maybe Color) String a
  | TextComponentBlur Cursor a
  | Commit a
  | Init a
  | GetValue (ValueHistory Color → a)
  | SetValue (ValueHistory Color) a


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
      { color: { current: initialColor, old: [] }
      , inputValues: mempty
      , props: _ }
  , render
  , eval
  , receiver: HE.input SetProps
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }

render ∷ ∀ m. State → HTML m
render state =
  let Root classes children = state.props.layout
  in
    HH.div
      [HP.classes classes] $
      fold $ mapWithIndex (\idx → renderLayout state $ List.Cons idx List.Nil) children


renderLayout ∷ ∀ m. State → Cursor → ChildLayout → Array (HTML m)
renderLayout state@{ color, inputValues, props} cursor = case _ of
  Group classes l → pure $
    HH.div
      [ HP.classes classes ]
      $ fold $ mapWithIndex (\idx → renderLayout state (List.Cons idx cursor)) l
  Component c → case c of
    ActionComponentSpec view → view
      { color
      , setColor: H.action <<< UpdateCurrentColor
      , commit: H.action Commit
      }
    DragComponentSpec spec → spec.view
      { color: color.current
      , onMouseDown: Left >>> DragStart spec.update >>> H.action
      , onTouchStart: Right >>> DragStart spec.update >>> H.action
      }
    NumberComponentSpec spec → spec.view
      { color: color.current
      , input: HH.slot'
          cpNumComponent
          cursor
          Num.input
          spec.props
          (HE.input \(Num.NotifyChange val) → NumberComponentUpdate cursor val)
      }
    TextComponentSpec spec → spec.view
      { color: color.current
      , value: lookup cursor inputValues
      , onValueInput: TextComponentUpdate cursor spec.fromString >>> H.action
      , onBlur: const (TextComponentBlur cursor) >>> H.action
      }


eval ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ Query ~> DSL m
eval = case _ of
  TextComponentBlur cursor next → do
    state ← H.get
    let val = lookup cursor state.inputValues
    for_ (lookup cursor state.inputValues) \val →
      when val.isValid $ H.modify _ { inputValues = mempty ∷ Map Cursor InputTextValue }
    pure next
  TextComponentUpdate cursor fromString str next → do
    let color = fromString str
    state ← H.get
    for_ color $ updateColor $ state
      { inputValues = insert
          cursor
          { isValid: isJust color
          , value: str
          }
          state.inputValues
      }
    pure next
  NumberComponentUpdate cursor num next → do
    state ← H.get
    case focus cursor state.props.layout of
      Just (Component (NumberComponentSpec { update })) →
        for_ (update <$> num >>= (_ $ state.color.current)) $ updateColor state
      _ → pure unit
    pure next
  SetValue val next → do
    H.modify _{ color = val }
    pure next
  GetValue next → H.gets $ _.color >>> next
  Init next → do
    propagate
    pure next
  Commit next → do
    state ← H.get
    H.put $ state
      { color =
        { current: state.color.current
        , old: nub $ [state.color.current] <> state.color.old
        }
      }
    H.raise $ NotifyChange state.color.current
    pure next
  UpdateCurrentColor color next → do
    state ← H.get
    updateColor state color
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
  H.put state{ color { current = colorNext } }
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
  propagateLayout ∷ Color → Cursor → Layout → DSL m Unit
  propagateLayout color cursor = case _ of
    Root _ l → void $ sequence $ mapWithIndex (\idx → propagateChildLayout color (List.Cons idx cursor)) l
  propagateChildLayout ∷ Color → Cursor → ChildLayout → DSL m Unit
  propagateChildLayout color cursor = case _ of
    Group _ l → void $ sequence $ mapWithIndex (\idx → propagateChildLayout color (List.Cons idx cursor)) l
    Component c → case c of
      DragComponentSpec _ → pure unit
      TextComponentSpec _ → pure unit
      ActionComponentSpec _ → pure unit
      NumberComponentSpec { read } →
        H.query' cpNumComponent cursor (H.action $ Num.SetValue $ Just $ read color) >>= mustBeMounted

mustBeMounted ∷ ∀ s f g p o m a. Maybe a → H.HalogenM s f g p o m a
mustBeMounted (Just x) = pure x
mustBeMounted _ = halt "children must be mounted"

type Cursor = List.List Int

focus ∷ Cursor → Layout → Maybe ChildLayout
focus cursor layout =
  let Root x children = layout
  in foldr f (Just $ Group x children) cursor
  where
  f idx = case _ of
    Just (Group _ l) → index l idx
    _ → Nothing
