module Halogen.ColorPicker.SVDrag where

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
        let hsva = C.toHSVA st.color
        let newS = dragData.progress.x
        let newV = 1.0 - dragData.progress.y
        let newColor = C.hsva hsva.h newS newV hsva.a
        H.modify (_{ color = newColor })
        H.raise newColor
      HCD.Done _ → pure unit
    pure next


render
  ∷ Array (HP.IProp I.HTMLdiv (Query Unit))
  → Array (HP.IProp I.HTMLdiv (Query Unit))
  → State
  → H.ComponentHTML Query
render props pointerProps state =
  HH.div
  ( props
    <> [ HCSS.style $ CSS.backgroundColor $ C.hsl hsv.h 1.0 0.5
       , HE.onTouchStart $ HE.input DragStart <<< Right
       , HE.onMouseDown $ HE.input DragStart <<< Left
       ] )
    [ HH.div
      ( pointerProps
        <> [ HCSS.style do
                CSS.left $ CSS.pct (hsv.s * 100.0)
                CSS.bottom $ CSS.pct (hsv.v * 100.0)
                CSS.backgroundColor state.color
           ])
      []
    ]
  where
  hsv = C.toHSVA state.color
