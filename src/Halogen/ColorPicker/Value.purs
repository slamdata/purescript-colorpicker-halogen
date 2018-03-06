module Halogen.ColorPicker.Value where

import Prelude

import Color as C
import Data.Const (Const)
import Global as G
import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.Proxy as HCP

data Query a
  = Receive C.Color a
  | Update String a

type State =
  { input ∷ String
  , color ∷ C.Color
  }

component'
  ∷ ∀ m
  . Array (HP.IProp I.HTMLinput (Query Unit))
  → HCP.ProxyComponent (Const Void) C.Color C.Color m
component' props = HCP.proxy $ H.component
  { initialState: \color → { color, input: show $ (C.toHSVA color).a }
  , render: render props
  , eval
  , receiver: HE.input Receive
  }

component ∷ ∀ m. HCP.ProxyComponent (Const Void) C.Color C.Color m
component = component' []

eval ∷ ∀ m. Query ~> H.ComponentDSL State Query C.Color m
eval = case _ of
  Receive c next → do
    st ← H.get
    unless (st.color == c)
      $ H.put { color: c, input: show $ (C.toHSVA c).v }
    pure next
  Update str next → do
    st ← H.get
    H.modify (_{ input = str })
    let v = G.readFloat str
    when (not G.isNaN v && v >= 0.0 && v <= 1.0) do
      let hsva = C.toHSVA st.color
      let newColor = C.hsva hsva.h hsva.s v hsva.a
      H.modify (_{ color = newColor })
      H.raise newColor
    pure next

render ∷ Array (HP.IProp I.HTMLinput (Query Unit)) → State → H.ComponentHTML Query
render props state =
  HH.input
    $ props
    <> [ HP.value state.input
       , HE.onValueInput $ HE.input Update
       , HP.type_ HP.InputNumber
       , HP.min 0.0
       , HP.max 1.0
       ]
