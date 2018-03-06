module Main where

import Prelude

import Color as C
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Data.Const (Const)
import Data.Either as E
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Maybe (Maybe(..))
import Debug.Trace as DT
import Halogen as H
import Halogen.Aff as HA
import Halogen.ColorPicker.Alpha as Alpha
import Halogen.ColorPicker.Blue as Blue
import Halogen.ColorPicker.Copy as Copy
import Halogen.ColorPicker.DragEventSource as HCD
import Halogen.ColorPicker.Green as Green
import Halogen.ColorPicker.Hex as Hex
import Halogen.ColorPicker.Hue as Hue
import Halogen.ColorPicker.HueDrag as HueDrag
import Halogen.ColorPicker.SVDrag as SVDrag
import Halogen.ColorPicker.Luminosity as Luminosity
import Halogen.ColorPicker.Red as Red
import Halogen.ColorPicker.SaturationHSL as SaturationHSL
import Halogen.ColorPicker.SaturationHSV as SaturationHSV
import Halogen.ColorPicker.Value as Value
import Halogen.Component.ChildPath as CP
import Halogen.Component.Proxy as HCP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main ∷ Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  _ ← runUI example unit body
  pure unit

infixr 6 type E.Either as <+>

data Query a = HandleMsg C.Color a

type ColorQuery = HCP.ProxyQ (Const Void) C.Color C.Color

type ChildQuery =
  ColorQuery <\/> Const Void

type Slot = Int <+> Void

type HTML m = H.ParentHTML Query ChildQuery Slot m
type DSL m = H.ParentDSL C.Color Query ChildQuery Slot Void m

example ∷ ∀ m r. MonadAff (HCD.DragEffects r) m ⇒ H.Component HH.HTML Query Unit Void m
example = H.parentComponent
  { initialState: const C.black
  , render
  , eval
  , receiver: const Nothing
  }

render ∷ ∀ m r. MonadAff (HCD.DragEffects r) m ⇒ C.Color → HTML m
render c =
  HH.div_
    [ HH.h3_ [ HH.text "HEX" ]
    , HH.slot' CP.cp1 0 Hex.component c $ HE.input HandleMsg
    , HH.hr_
    , HH.h3_ [ HH.text "COPY" ]
    , HH.slot' CP.cp1 1 (Copy.component' [ HP.class_ $ HH.ClassName "ColorPicker-copy" ]) c $ HE.input HandleMsg
    , HH.hr_
    , HH.h3_ [ HH.text "RGBA" ]
    , HH.slot' CP.cp1 2 Red.component c $ HE.input HandleMsg
    , HH.slot' CP.cp1 3 Green.component c $ HE.input HandleMsg
    , HH.slot' CP.cp1 4 Blue.component c $ HE.input HandleMsg
    , HH.slot' CP.cp1 5 Alpha.component c $ HE.input HandleMsg
    , HH.hr_
    , HH.h3_ [ HH.text "HSLA" ]
    , HH.slot' CP.cp1 6 Hue.component c $ HE.input HandleMsg
    , HH.slot' CP.cp1 7 SaturationHSL.component c $ HE.input HandleMsg
    , HH.slot' CP.cp1 8 Luminosity.component c $ HE.input HandleMsg
    , HH.slot' CP.cp1 9 Alpha.component c $ HE.input HandleMsg
    , HH.hr_
    , HH.h3_ [ HH.text "HSVA" ]
    , HH.slot' CP.cp1 10 Hue.component c $ HE.input HandleMsg
    , HH.slot' CP.cp1 11 SaturationHSV.component c $ HE.input HandleMsg
    , HH.slot' CP.cp1 12 Value.component c $ HE.input HandleMsg
    , HH.slot' CP.cp1 13 Alpha.component c $ HE.input HandleMsg
    , HH.hr_
    , HH.h3_ [ HH.text "Hue drag" ]
    , HH.slot' CP.cp1 14 (HueDrag.component' [ HP.class_ $ HH.ClassName "ColorPicker-slider" ] [ HP.class_ $ HH.ClassName "ColorPicker-sliderSelector"]) c $ HE.input HandleMsg
    , HH.hr_
    , HH.h3_ [ HH.text "SV drag" ]
    , HH.slot' CP.cp1 15 (SVDrag.component' [ HP.class_ $ HH.ClassName "ColorPicker-field"] [HP.class_ $ HH.ClassName "ColorPicker-fieldSelector"]) c $ HE.input HandleMsg
    ]

eval ∷ ∀ m. Query ~> DSL m
eval = case _ of
  HandleMsg c next → do
    H.put c
    pure next


--  pure unit
--  runUI example unit body
{-
data Query a = HandleMsg ColorIdx CPicker.Message a

type State = Map Int {current ∷ Color, next ∷ Color }
type ColorIdx = Int
type ChildQuery = Coproduct.Coproduct1 CPicker.Query
type Slot = Either.Either1 ColorIdx

cpColor ∷ CP.ChildPath CPicker.Query ChildQuery ColorIdx Slot
cpColor = CP.cp1

type HTML m = H.ParentHTML Query ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Void m


example ∷ ∀ m r. MonadAff (CPicker.PickerEffects r) m => H.Component HH.HTML Query Unit Void m
example = H.parentComponent
    { initialState: const mempty
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ ∀ m r. MonadAff (CPicker.PickerEffects r) m => State → HTML m
render state = HH.div [HP.class_ $ H.ClassName "root"]
  $ renderPicker orange 0 config0
  <> renderPicker blue 1 config1
  <> renderPicker red 2 config2
  <> renderPicker red 3 config3

  where
  renderPicker color idx conf =
    [ HH.h1_ [ HH.text $ "Picker " <> show idx ]
    , HH.slot' cpColor idx (CPicker.picker color) conf (HE.input $ HandleMsg idx)
    , HH.p_ [ HH.text case lookup idx state of
        Just ({current, next}) →
          "uncommited (current: " <> show current <>", next:" <> show next <> ")"
        Nothing → "no color"
      ]
    ]

eval ∷ ∀ m. Query ~> DSL m
eval (HandleMsg idx msg next) = do
  H.modify update
  pure next
  where
  update state = insert idx val state
    where
    val = case lookup idx state, msg of
      Just s, CPicker.NextChange nextVal → s{next = nextVal}
      _, CPicker.NextChange x → { next: x, current: x }
      _, CPicker.NotifyChange x →  {next: x, current: x}

config0 ∷ CPicker.Props
config0 = mkConf $ L.Root c $ reverse l
  where
  L.Root c l = mkLayout
    [H.ClassName "ColorPicker--small", H.ClassName "ColorPicker--inline"]
    [ [ L.componentHue
      , L.componentSaturationHSL
      , L.componentLightness
      ]
    ]

config1 ∷ CPicker.Props
config1 = mkConf $ mkLayout
  [H.ClassName "ColorPicker--large", H.ClassName "ColorPicker--inline"]
  [ [ L.componentHue
    , L.componentSaturationHSV
    , L.componentValue
    , L.componentSaturationHSL
    , L.componentLightness
    ]
  , [ L.componentRed
    , L.componentGreen
    , L.componentBlue
    , L.componentHEX
    ]
  ]

config2 ∷ CPicker.Props
config2 = mkConf $ mkLayout
  [H.ClassName "ColorPicker--small", H.ClassName "ColorPicker--inline"]
  [ [ const componentRedORNoRed ]]

config3 ∷ CPicker.Props
config3 = mkConf $ mkLayout
  [H.ClassName "ColorPicker--small", H.ClassName "ColorPicker--block"]
  [ [ const componentRedORNoRed ]]

componentRedORNoRed ∷ L.PickerComponent
componentRedORNoRed = L.TextComponentSpec
  { fromString: \str → guard (str == "red") $> red
  , view: \{color, value, onBlur, onValueInput } → pure $
      HH.label
        [ HP.classes inputClasses.root]
        [ HH.span [HP.classes inputClasses.label] [HH.text "🛑"]
        , HH.input
          [ HP.type_ HP.InputText
          , HP.classes
            $  inputClasses.elem
            <> (guard (L.isInvalid value) *> (inputClasses.elemInvalid))
          , HP.title "red or nored?"
          , HP.value $ maybe (toString color) _.value value
          , HP.placeholder "red"
          , HE.onValueInput $ onValueInput >>> Just
          , HE.onBlur $ onBlur >>> Just
          ]
        ]
  }
  where
  red = rgb 255 0 0
  toString = \color → if color == red then "red" else "noRed"


mkConf ∷ L.Layout → CPicker.Props
mkConf = { layout: _ }

mkLayout
  ∷ Array H.ClassName
  → Array (Array (L.InputProps → L.PickerComponent))
  → L.Layout
mkLayout root editGroups =
  ([ H.ClassName "ColorPicker"] <> root) `L.Root`
    [ [ H.ClassName "ColorPicker-dragger" ] `L.Group`
        [ L.Component $ L.componentDragSV
            { root: [ H.ClassName "ColorPicker-field" ]
            , isLight: [ H.ClassName "IsLight" ]
            , isDark: [ H.ClassName "IsDark" ]
            , selector: [ H.ClassName "ColorPicker-fieldSelector"]
            }
        , L.Component $ L.componentDragHue
            { root: [ H.ClassName "ColorPicker-slider" ]
            , selector: [ H.ClassName "ColorPicker-sliderSelector"]
            }
        ]
    , [ H.ClassName "ColorPicker-aside" ] `L.Group`
        [ [ H.ClassName "ColorPicker-stage" ] `L.Group`
            [ L.Component $ L.componentPreview [ H.ClassName "ColorPicker-colorBlockCurrent" ]
            , L.Component $ L.componentHistory 4 [ H.ClassName "ColorPicker-colorBlockOld" ]
            ]
        , L.Group [ H.ClassName "ColorPicker-editing" ] $
            editGroups <#> \editGroup →
              L.Group [ H.ClassName "ColorPicker-editingItem" ] $
                editGroup <#> \mkItem → L.Component $ mkItem inputClasses
        , [ H.ClassName "ColorPicker-actions" ] `L.Group`
            [ L.Component $ L.componentSet [ H.ClassName "ColorPicker-actionSet" ] ]
        ]
    ]

inputClasses ∷ L.InputProps
inputClasses =
  { root: [H.ClassName "ColorPicker-input"]
  , label: [H.ClassName "ColorPicker-inputLabel"]
  , elem: [H.ClassName "ColorPicker-inputElem"]
  , elemInvalid: [H.ClassName "ColorPicker-inputElem--invalid"]
  }
-}
