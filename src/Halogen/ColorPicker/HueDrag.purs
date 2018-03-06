module Halogen.ColorPicker.HueDrag where

import Prelude

import CSS as CSS
import Color as C
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Class (liftEff)
import DOM.Classy.Event as DCE
import DOM.HTML.Indexed as I
import Data.Const (Const)
import Data.Either (either, Either(..))
import Halogen as H
import Halogen.ColorPicker.DragEventSource as HCD
import Halogen.Component.Proxy as HCP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a
  = Receive C.Color a
  | DragStart HCD.CursorEvent a
  | DragMove HCD.DragEvent a

type State =
  { color ∷ C.Color
  }

component'
  ∷ ∀ m e
  . MonadAff (HCD.DragEffects e) m
  ⇒ Array (HP.IProp I.HTMLdiv (Query Unit))
  → Array (HP.IProp I.HTMLdiv (Query Unit))
  → HCP.ProxyComponent (Const Void) C.Color C.Color m
component' props sliderProps = HCP.proxy $ H.component
  { initialState: \color → { color }
  , render: render props sliderProps
  , eval
  , receiver: HE.input Receive
  }

component
  ∷ ∀ m e
  . MonadAff (HCD.DragEffects e) m
  ⇒ HCP.ProxyComponent (Const Void) C.Color C.Color m
component = component' [] []

eval
  ∷ ∀ m e
  . MonadAff (HCD.DragEffects e) m
  ⇒ Query
  ~> H.ComponentDSL State Query C.Color m
eval = case _ of
  Receive c next → do
    st ← H.get
    unless (st.color == c)
      $ H.put { color: c }
    pure next
  DragStart event next → do
    H.subscribe $ HCD.dragEventSource event \drag →
      pure $ DragMove drag H.Listening
    initialDragData ← liftEff do
      either DCE.preventDefault DCE.preventDefault event
      HCD.mkFirstDragData event
    eval $ DragMove (HCD.Move event initialDragData) next
  DragMove drag next → do
    case drag of
      HCD.Move event dragData → do
        st ← H.get
        let hsla = C.toHSLA st.color
        let newH = 360.0 * (1.0 - dragData.progress.y)
        let newColor = C.hsla newH hsla.s hsla.l hsla.a
        H.modify (_{ color = newColor })
        H.raise newColor
      HCD.Done _ → pure unit
    pure next

render
  ∷ Array (HP.IProp I.HTMLdiv (Query Unit))
  → Array (HP.IProp I.HTMLdiv (Query Unit))
  → State
  → H.ComponentHTML Query
render props sliderProps state =
  HH.div
    ( props
      <> [ HE.onTouchStart $ HE.input DragStart <<< Right
         , HE.onMouseDown $ HE.input DragStart <<< Left
         ]
    )
    [ HH.div ( sliderProps <> [ HCSS.style $ CSS.top $ CSS.pct ((1.0 - (C.toHSVA state.color).h / 360.0) * 100.0) ] )
      [ ]
    ]
