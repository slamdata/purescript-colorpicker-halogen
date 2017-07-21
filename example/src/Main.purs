module Main where

import Prelude

import ColorPicker.Halogen.Component as CPicker
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Class (class MonadAff)
import Data.Either.Nested as Either
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Maybe (Maybe(..))
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
render _ = HH.div_ $ 
  -- [ HH.h1_ [ HH.text "Picker 1" ]] <>
  [ HH.slot' cpColor 0 CPicker.picker {mainRectWidth: 200, hueRectWidth: 30} (HE.input (HandleMsg 0))] <>
  [ HH.slot' cpColor 1 CPicker.picker {mainRectWidth: 300, hueRectWidth: 30} (HE.input (HandleMsg 1))]

eval ∷ ∀ m. Query ~> DSL m
eval (HandleMsg _ _ next) = pure next
