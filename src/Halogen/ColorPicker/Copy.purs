module Halogen.ColorPicker.Copy where

import Prelude

import Color as C
import CSS as CSS
import Data.Const (Const)
import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.Proxy as HCP

data Query a
  = Receive C.Color a
  | Copy a

component'
  ∷ ∀ m
  . Array (HP.IProp I.HTMLdiv (Query Unit))
  → HCP.ProxyComponent (Const Void) C.Color C.Color m
component' props = HCP.proxy $ H.component
  { initialState: id
  , render: render props
  , eval
  , receiver: HE.input Receive
  }

component ∷ ∀ m. HCP.ProxyComponent (Const Void) C.Color C.Color m
component = component' []

eval ∷ ∀ m. Query ~> H.ComponentDSL C.Color Query C.Color m
eval = case _ of
  Receive c next → do
    H.put c
    pure next
  Copy next → do
    st ← H.get
    H.raise st
    pure next

render ∷ Array (HP.IProp I.HTMLdiv (Query Unit)) → C.Color → H.ComponentHTML Query
render props color =
  HH.div
    ( props <> [ HCSS.style $ CSS.backgroundColor color, HE.onClick $ HE.input_ Copy ] )
    [ ]
