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
import ColorPicker.Halogen.ColorComponents (ColorComponent(..), ColorEnv, PositionUpdate, PreNumConf, InputClasses)
import ColorPicker.Halogen.Layout as L
import ColorPicker.Halogen.Utils.Drag as Drag
import Control.Monad.Aff.Class (class MonadAff)
import Control.MonadZero (guard)
import DOM.Classy.Event (preventDefault)
import Data.Either (Either(..), either, isLeft)
import Data.Either.Nested as Either
import Data.Foldable (foldMap, for_)
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
render state@{ color, inputs, props} = renderLayout state props.layout

renderLayout ∷ ∀ m. State → L.Layout → HTML m
renderLayout state@{ color, inputs, props} = case _ of
  L.Group classes l →
    HH.div
      [ HP.classes classes ]
      $ map (renderLayout state) l
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
          [ HP.classes $ (props `classesFor` ColorBlockNext) <> colorClasses props color.next
          , HP.title "Next value"
          , HCSS.style $ CSS.backgroundColor color.next
          ]
          []
      , HH.div
          [ HP.tabIndex 0
          , HP.classes $ (props `classesFor` ColorBlockCurrent) <> colorClasses props color.current
          , HP.title "Current value"
          , HE.onClick $ HE.input (\_ → ComponentUpdate $ const $ Just color.current)
          , HCSS.style $ CSS.backgroundColor color.current
          ]
          []
      ]
  L.Component c → renderColorComponent state c

  where
  hsv = Color.toHSVA $ color.next
  colorEnv = calcColorEnv color.next
  renderColorComponent ∷ State → ColorComponent → HTML m
  renderColorComponent { inputs, props } = case _ of
    DragComponentSpec spec →
      let
        root = spec.root colorEnv
        selector = spec.selector colorEnv
      in
        HH.div
          [ HP.classes root.classes
          , HCSS.style $ root.css
          , HE.onMouseDown $ HE.input (Left >>> DragStart spec.update)
          , HE.onTouchStart $ HE.input (Right >>> DragStart spec.update)
          ]
          [ HH.div
            [ HP.classes $ selector.classes
            , HCSS.style $ selector.css
            ]
            []
          ]
    NumberComponentSpec { classes, key, hasNumVal, update, config } →
      input classes config.prefix
        $ pure
        $ HH.slot' cpNumComponent key (Num.input hasNumVal (mkNumConf classes config)) unit
        $ HE.input \(Num.NotifyChange val) → ComponentUpdate $ \color → update <$> val >>= (_ $ color)
    TextComponentSpec { classes, fromString, key, config } →
      input classes config.prefix $ flip foldMap (lookup key inputs) \val → pure $ HH.input
        [ HP.type_ HP.InputText
        , HP.classes
          $  classes.elem
          <> (guard (isLeft val) *> (classes.elemInvalid))
        , HP.title config.title
        , HP.placeholder config.placeholder
        , HP.value $ either id id val
        , HE.onValueInput $ HE.input $ \ str → UpdateTextInput key str (fromString str)
        ]
    where
    input classes label child =
      HH.label [HP.classes classes.root] $
        [ HH.span [HP.classes classes.label] [HH.text label]] <> child

colorClasses ∷ Props → Color → Array HH.ClassName
colorClasses props c = props `classesFor` (if Color.isLight c then IsLight else IsDark)

mkNumConf ∷ ∀ a. InputClasses → PreNumConf a → Num.Config a
mkNumConf {elem, elemInvalid} { title, placeholder, range } =
  { title
  , placeholder
  , range
  , root: elem
  , rootInvalid: elemInvalid
  , rootLength: const []
  }

eval ∷ ∀ m r. MonadAff (PickerEffects r) m ⇒ Query ~> DSL m
eval = case _ of
  UpdateTextInput key str color next → do
    let
      val = case color of
        Nothing → Left str
        Just _ → Right str
    H.modify \s → s { inputs = insert key val s.inputs }
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
  DragMove update drag next → do
    case drag of
      Drag.Move event dragData → do
        state ← H.get
        updateColor state $ update dragData.progress state.color.next
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
    color
    (calcColorEnv color.next)
    layout
  where
  propagateLayout ∷ ValueProgress Color → ColorEnv → L.Layout → DSL m Unit
  propagateLayout color colorEnv = case _ of
    L.Group _ l → for_ l (propagateLayout color colorEnv)
    L.Stage → pure unit
    L.Actions → pure unit
    L.Component c → case c of
      DragComponentSpec _ → pure unit
      TextComponentSpec { key, toString } →
        H.modify \s → s { inputs = insert key (Right $ toString color.next) s.inputs }
      NumberComponentSpec {key, read} →
        H.query' cpNumComponent key (H.action $ Num.SetValue $ Just $ read colorEnv) >>= mustBeMounted

mustBeMounted ∷ ∀ s f g p o m a. Maybe a → H.HalogenM s f g p o m a
mustBeMounted (Just x) = pure x
mustBeMounted _ = halt "children must be mounted"
