module Main where

import Prelude

import Color as C
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Data.Const (Const)
import Data.Either as E
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.FoldableWithIndex as FI
import Data.Maybe (Maybe(..))
import Data.List as L
import Debug.Trace as DT
import Halogen as H
import Halogen.Aff as HA
import Halogen.ColorPicker.Alpha as Alpha
import Halogen.ColorPicker.Blue as Blue
import Halogen.ColorPicker.Copy as Copy
import Halogen.ColorPicker.DragEventSource as HCD
import Halogen.ColorPicker.Green as Green
import Halogen.ColorPicker.Hex as Hex
import Halogen.ColorPicker.Hue as Hue
import Halogen.ColorPicker.HueDrag as HueDrag
import Halogen.ColorPicker.Luminosity as Luminosity
import Halogen.ColorPicker.Red as Red
import Halogen.ColorPicker.SVDrag as SVDrag
import Halogen.ColorPicker.SaturationHSL as SaturationHSL
import Halogen.ColorPicker.SaturationHSV as SaturationHSV
import Halogen.ColorPicker.Value as Value
import Halogen.ColorPicker.Common as HCC
import Halogen.Component.ChildPath as CP
import Halogen.Component.Proxy as HCP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main ∷ Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  _ ← runUI example unit body
  pure unit

infixr 6 type E.Either as <+>

data Query a
  = HandleMsg C.Color a
  | AddToPalette a

type ChildQuery =
  HCC.ColorModifierQ <\/> HCC.ColorModifierQ <\/> Const Void

type Slot = Int <+> Int <+> Void

type State =
  { color ∷ C.Color
  , palette ∷ L.List C.Color
  }

type HTML m = H.ParentHTML Query ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Void m

example ∷ ∀ m r. MonadAff (HCD.DragEffects r) m ⇒ H.Component HH.HTML Query Unit Void m
example = H.parentComponent
  { initialState: const { color: C.black, palette: L.Nil }
  , render
  , eval
  , receiver: const Nothing
  }

render ∷ ∀ m r. MonadAff (HCD.DragEffects r) m ⇒ State → HTML m
render {color: c, palette} =
  HH.div_
    $ [ HH.h3_ [ HH.text "HEX" ]
      , HH.slot' CP.cp1 0 Hex.component c $ HE.input HandleMsg
      , HH.hr_
      , HH.h3_ [ HH.text "COPY" ]
      , HH.slot' CP.cp1 1 copyComponent c  $ HE.input HandleMsg
      , HH.hr_
      , HH.h3_ [ HH.text "RGBA" ]
      , HH.slot' CP.cp1 2 Red.component c $ HE.input HandleMsg
      , HH.slot' CP.cp1 3 Green.component c $ HE.input HandleMsg
      , HH.slot' CP.cp1 4 Blue.component c $ HE.input HandleMsg
      , HH.slot' CP.cp1 5 Alpha.component c $ HE.input HandleMsg
      , HH.hr_
      , HH.h3_ [ HH.text "HSLA" ]
      , HH.slot' CP.cp1 6 Hue.component c $ HE.input HandleMsg
      , HH.slot' CP.cp1 7 SaturationHSL.component c $ HE.input HandleMsg
      , HH.slot' CP.cp1 8 Luminosity.component c $ HE.input HandleMsg
      , HH.slot' CP.cp1 9 Alpha.component c $ HE.input HandleMsg
      , HH.hr_
      , HH.h3_ [ HH.text "HSVA" ]
      , HH.slot' CP.cp1 10 Hue.component c $ HE.input HandleMsg
      , HH.slot' CP.cp1 11 SaturationHSV.component c $ HE.input HandleMsg
      , HH.slot' CP.cp1 12 Value.component c $ HE.input HandleMsg
      , HH.slot' CP.cp1 13 Alpha.component c $ HE.input HandleMsg
      , HH.hr_
      , HH.h3_ [ HH.text "Hue drag" ]
      , HH.slot' CP.cp1 14 (HueDrag.component' [ HP.class_ $ HH.ClassName "ColorPicker-slider" ] [ HP.class_ $ HH.ClassName "ColorPicker-sliderSelector"]) c $ HE.input HandleMsg
      , HH.hr_
      , HH.h3_ [ HH.text "SV drag" ]
      , HH.slot' CP.cp1 15 (SVDrag.component' [ HP.class_ $ HH.ClassName "ColorPicker-field"] [HP.class_ $ HH.ClassName "ColorPicker-fieldSelector"]) c $ HE.input HandleMsg
      , HH.hr_
      , HH.h3_ [ HH.text "Palette" ]
      , HH.button [ HE.onClick $ HE.input_ AddToPalette ] [ HH.text "add to palette" ]
      ]
    <> FI.foldMapWithIndex foldFn palette
  where
  copyComponent = Copy.component' [ HP.class_ $ HH.ClassName "ColorPicker-copy" ]

  foldFn ix col =
    [ HH.slot' CP.cp2 ix copyComponent col $ HE.input HandleMsg ]

eval ∷ ∀ m. Query ~> DSL m
eval = case _ of
  HandleMsg c next → do
    H.modify (_{ color = c })
    pure next
  AddToPalette next → do
    st ← H.get
    unless (L.elem st.color st.palette) do
      H.modify (_{ palette = L.Cons st.color st.palette })
    pure next
