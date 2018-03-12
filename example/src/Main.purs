module Main where

import Prelude

import Color as C
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Data.Const (Const)
import Data.Either as E
import Data.FoldableWithIndex as FI
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.List as L
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.ColorPicker.Alpha as Alpha
import Halogen.ColorPicker.Blue as Blue
import Halogen.ColorPicker.Common as HCC
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
import Halogen.Component.ChildPath as CP
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
  HH.div [ HP.classes [ H.ClassName "ColorPicker", H.ClassName "ColorPicker--large", H.ClassName "ColorPicker--inline" ]]
    $ [ HH.div [ HP.classes [ H.ClassName "ColorPicker-dragger" ]]
          [ HH.slot' CP.cp1 15 (SVDrag.component' [ HP.class_ $ HH.ClassName "ColorPicker-field"] [HP.class_ $ HH.ClassName "ColorPicker-fieldSelector"]) c $ HE.input HandleMsg
          , HH.slot' CP.cp1 14 (HueDrag.component' [ HP.class_ $ HH.ClassName "ColorPicker-slider" ] [ HP.class_ $ HH.ClassName "ColorPicker-sliderSelector"]) c $ HE.input HandleMsg
          ]
      , HH.div [ HP.classes [ H.ClassName "ColorPicker-aside" ]]
          [ HH.div [ HP.classes [ H.ClassName "ColorPicker-stage" ]]
            $ [ HH.slot' CP.cp1 1 (Copy.component' [ HP.class_ $ HH.ClassName "ColorPicker-colorBlockCurrent" ]) c $ HE.input HandleMsg
              ]
              <> FI.foldMapWithIndex foldFn palette
          , HH.div [ HP.classes [ H.ClassName "ColorPicker-editing" ]]
              [ HH.div [ HP.classes [ H.ClassName "ColorPicker-editingItem" ]]
                  [ input "H" $ \p -> HH.slot' CP.cp1 6 (Hue.component' p) c $ HE.input HandleMsg
                  , input "ʜSᴠ" $ \p -> HH.slot' CP.cp1 11 (SaturationHSV.component' $ p <> [HP.step $ HP.Step 0.01]) c $ HE.input HandleMsg
                  , input "V" $ \p -> HH.slot' CP.cp1 12 (Value.component' $ p <> [HP.step $ HP.Step 0.01]) c $ HE.input HandleMsg
                  , input "ʜSʟ" $ \p -> HH.slot' CP.cp1 7 (SaturationHSL.component' $ p <> [HP.step $ HP.Step 0.01]) c $ HE.input HandleMsg
                  , input "L" $ \p -> HH.slot' CP.cp1 8 (Luminosity.component' $ p <> [HP.step $ HP.Step 0.01]) c $ HE.input HandleMsg
                  ]
              , HH.div [ HP.classes [ H.ClassName "ColorPicker-editingItem" ]]
                  [ input "R" $ \p -> HH.slot' CP.cp1 2 (Red.component' p) c $ HE.input HandleMsg
                  , input "G" $ \p -> HH.slot' CP.cp1 3 (Green.component' p) c $ HE.input HandleMsg
                  , input "B" $ \p -> HH.slot' CP.cp1 4 (Blue.component' p) c $ HE.input HandleMsg
                  , input "α" $ \p -> HH.slot' CP.cp1 5 (Alpha.component' $ p <> [HP.step $ HP.Step 0.01]) c $ HE.input HandleMsg
                  , input "#" $ \p -> HH.slot' CP.cp1 0 (Hex.component' p) c $ HE.input HandleMsg
                  ]
              ]
          , HH.div [ HP.classes [ H.ClassName "ColorPicker-actions" ]]
              [ HH.button [ HP.classes [ H.ClassName "ColorPicker-actionSet" ], HE.onClick $ HE.input_ AddToPalette ] [ HH.text "Set" ]
              ]
          ]
      ]
  where
  input label elem =
    HH.div [ HP.classes [ H.ClassName "ColorPicker-input" ]]
      [ HH.label [ HP.classes [ H.ClassName "ColorPicker-inputLabel" ]] [ HH.text label ]
      , elem [ HP.classes [ H.ClassName "ColorPicker-inputElem" ]]
      ]
  foldFn ix col = [ HH.slot' CP.cp2 ix (Copy.component' [ HP.class_ $ HH.ClassName "ColorPicker-colorBlockOld" ]) col $ HE.input HandleMsg ]

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
