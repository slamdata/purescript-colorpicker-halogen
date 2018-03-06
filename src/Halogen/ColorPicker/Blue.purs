module Halogen.ColorPicker.Blue where

import Prelude

import Color as C
import Control.MonadZero (guard)
import Data.Const (Const)
import Data.Foldable as F
import Data.Int as Int
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
  { initialState: \color → { color, input: show $ (C.toRGBA color).b }
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
      $ H.put { color: c, input: show $ (C.toRGBA c).b }
    pure next
  Update s next → do
    st ← H.get
    H.modify (_{ input = s })
    let mbb = do
          i ← Int.fromString s
          guard $ i < 256 && i > -1
          pure i
    F.for_ mbb \b → do
      let rgba = C.toRGBA st.color
      let newColor = C.rgba rgba.r rgba.g b rgba.a
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
       , HP.step $ HP.Step 1.0
       , HP.min 0.0
       , HP.max 255.0
       ]
