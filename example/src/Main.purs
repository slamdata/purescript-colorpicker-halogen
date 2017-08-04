module Main where

import Prelude

import Color (Color)
import ColorPicker.Halogen.Component as CPicker
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Data.Either.Nested as Either
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main ∷ Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI example unit body

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
render state = HH.div_
  $ renderPicker 0 config0
  <> renderPicker 1 config1

  where
  renderPicker idx conf =
    [ HH.h1_ [ HH.text $ "Picker " <> show idx ]
    , HH.slot' cpColor idx CPicker.picker conf (HE.input $ HandleMsg idx)
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
      Just s, CPicker.NextChange next -> s{next = next}
      _, CPicker.NextChange x -> { next: x, current: x }
      _, CPicker.NotifyChange x ->  {next: x, current: x}

config0 ∷ CPicker.Props
config0 = mkConf
  [ClassName "ColorPicker--small"]
  [CPicker.componentHue <> CPicker.componentSL]

config1 ∷ CPicker.Props
config1 = mkConf
  [ClassName "ColorPicker--large"]
  [CPicker.componentHue <> CPicker.componentSV <> CPicker.componentHEX, CPicker.componentRGB]

mkConf ∷ Array ClassName → CPicker.ColorComponentGroups → CPicker.Props
mkConf root editing =
  { editing
  , classes: fromFoldable
    [ Tuple CPicker.Root $ [ClassName "ColorPicker"] <> root
    , Tuple CPicker.Dragger [ClassName "ColorPicker-dragger"]
    , Tuple CPicker.Field [ClassName "ColorPicker-field"]
    , Tuple CPicker.FieldGradient [ClassName "ColorPicker-fieldGradient"]
    , Tuple CPicker.FieldSelector [ClassName "ColorPicker-fieldSelector"]
    , Tuple CPicker.Slider [ClassName "ColorPicker-slider"]
    , Tuple CPicker.SliderSelector [ClassName "ColorPicker-sliderSelector"]
    , Tuple CPicker.Aside [ClassName "ColorPicker-aside"]
    , Tuple CPicker.Stage [ClassName "ColorPicker-stage"]
    , Tuple CPicker.ColorBlockCurrent [ClassName "ColorPicker-colorBlockCurrent"]
    , Tuple CPicker.ColorBlockPrevious [ClassName "ColorPicker-colorBlockPrevious"]
    , Tuple CPicker.Editing [ClassName "ColorPicker-editing"]
    , Tuple CPicker.EditingItem [ClassName "ColorPicker-editingItem"]
    , Tuple CPicker.Input [ClassName "ColorPicker-input"]
    , Tuple CPicker.InputLabel [ClassName "ColorPicker-inputLabel"]
    , Tuple CPicker.InputElem [ClassName "ColorPicker-inputElem"]
    , Tuple CPicker.InputElemInvalid [ClassName "ColorPicker-inputElem--invalid"]
    , Tuple CPicker.Actions [ClassName "ColorPicker-actions"]
    , Tuple CPicker.ActionSet [ClassName "ColorPicker-actionSet"]
    ]
  }
