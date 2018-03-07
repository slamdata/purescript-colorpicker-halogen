module Halogen.ColorPicker.Hue where

import Prelude

import Data.Lens (view)
import Halogen as H
import Halogen.ColorPicker.Common as HCC
import Halogen.Component.Proxy as HCP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

component' ∷ ∀ m. HCC.InputProps → HCC.ColorModifier m
component' props = HCP.proxy $ H.component
  { initialState: view HCC._Hue
  , render: render props
  , eval: HCC.eval HCC._Hue
  , receiver: HE.input HCC.Receive
  }

component ∷ ∀ m. HCC.ColorModifier m
component = component' []

render ∷ HCC.InputProps → HCC.InputState → HCC.InputHTML
render props state =
  HH.input
    $ props
    <> [ HP.value state.input
       , HE.onValueInput $ HE.input HCC.Update
       , HP.type_ HP.InputNumber
       ]
