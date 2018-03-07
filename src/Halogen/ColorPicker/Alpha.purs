module Halogen.ColorPicker.Alpha where

import Prelude

import Halogen.ColorPicker.Common as HCC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

component' ∷ ∀ m. HCC.InputProps → HCC.ColorModifier m
component' props = HCC.inputComponent HCC._Alpha $ render props

component ∷ ∀ m. HCC.ColorModifier m
component = component' []

render ∷ HCC.InputProps → HCC.InputState → HCC.InputHTML
render props state =
  HH.input
    $ props
    <> [ HP.value state.input
       , HE.onValueInput $ HE.input HCC.Update
       , HP.type_ HP.InputNumber
       , HP.min 0.0
       , HP.max 1.0
       ]
