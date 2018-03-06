module Halogen.ColorPicker.Hex where

import Prelude

import Color as C
import Data.Const (Const)
import Data.Foldable as F
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
  { initialState: \color → { color, input: C.toHexString color }
  , render: render props
  , eval
  , receiver: HE.input Receive
  }

component ∷ ∀ m. HCP.ProxyComponent (Const Void) C.Color C.Color m
component = component' []

eval ∷ ∀ m. Query ~> H.ComponentDSL State Query C.Color m
eval = case _ of
  Receive color next → do
    st ← H.get
    unless (st.color == color)
      $ H.put { color, input: C.toHexString color }
    pure next
  Update s next → do
    H.modify (_{ input = s })
    F.for_ (C.fromHexString s) \c → do
      st ← H.get
      let newHSLA = C.toHSLA c
      let newColor = C.hsla newHSLA.h newHSLA.s newHSLA.l (_.a $ C.toHSLA st.color)
      H.modify (_{ color = newColor })
      H.raise newColor
    pure next

render ∷ Array (HP.IProp I.HTMLinput (Query Unit)) → State → H.ComponentHTML Query
render props state =
  HH.input $ props <> [ HP.value state.input, HE.onValueInput $ HE.input Update ]
