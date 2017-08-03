module Main where

import Prelude

import ColorPicker.Halogen.Component as CPicker
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Data.Either.Nested as Either
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Maybe (Maybe(..))
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

type State = {}
type ColorIdx = Int
type ChildQuery = Coproduct.Coproduct1 CPicker.Query
type Slot = Either.Either1 ColorIdx


cpColor ∷ CP.ChildPath CPicker.Query ChildQuery ColorIdx Slot
cpColor = CP.cp1


type HTML m = H.ParentHTML Query ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Void m


example ∷ ∀ m r. MonadAff (CPicker.PickerEffects r) m => H.Component HH.HTML Query Unit Void m
example = H.parentComponent
    { initialState: const {}
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ ∀ m r. MonadAff (CPicker.PickerEffects r) m => State → HTML m
render _ = HH.div_
  $  [ HH.h1_ [ HH.text "Picker 1" ]]
  <> [ HH.slot' cpColor 0 CPicker.picker config0 (HE.input (HandleMsg 0))]
  <> [ HH.h1_ [ HH.text "Picker 2" ]]
  <> [ HH.slot' cpColor 1 CPicker.picker config1 (HE.input (HandleMsg 1))]

eval ∷ ∀ m. Query ~> DSL m
eval (HandleMsg _ _ next) = pure next

config0 ∷ CPicker.Props
config0 = mkConf
  [ClassName "ColorPicker--small"]
  [CPicker.componentHue <> CPicker.componentSL]

config1 ∷ CPicker.Props
config1 = mkConf
  [ClassName "ColorPicker--large"]
  [CPicker.componentHue <> CPicker.componentSV <> CPicker.componentHEX, CPicker.componentRGB]

mkConf ∷ Array ClassName -> CPicker.ColorComponentGroups -> CPicker.Props
mkConf root editing =
  { rootClasses: [ClassName "ColorPicker"] <> root
  , draggerClasses: [ClassName "ColorPicker-dragger"]
  , fieldClasses: [ClassName "ColorPicker-field"]
  , fieldGradientClasses: [ClassName "ColorPicker-fieldGradient"]
  , fieldSelectorClasses: [ClassName "ColorPicker-fieldSelector"]
  , sliderClasses: [ClassName "ColorPicker-slider"]
  , sliderSelectorClasses: [ClassName "ColorPicker-sliderSelector"]
  , editingClasses: [ClassName "ColorPicker-editing"]
  , editingItemClasses: [ClassName "ColorPicker-editingItem"]
  , inputClasses: [ClassName "ColorPicker-input"]
  , inputLabelClasses: [ClassName "ColorPicker-inputLabel"]
  , inputElemClasses: [ClassName "ColorPicker-inputElem"]
  , inputElemInvalidClasses: [ClassName "ColorPicker-inputElem--invalid"]
  , editing
  }
