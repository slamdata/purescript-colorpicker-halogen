module Halogen.ColorPicker.Common where

import Prelude

import Color as C
import Control.MonadZero (guard)
import DOM.HTML.Indexed as I
import Data.Const (Const)
import Data.Int as Int
import Data.Lens (Iso', iso, view, review)
import Data.Maybe as M
import Data.Record as Rec
import Global as G
import Halogen as H
import Halogen.Component.Proxy as HCP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type ColorModifier m = HCP.ProxyComponent (Const Void) C.Color C.Color m

type ColorModifierQ = HCP.ProxyQ (Const Void) C.Color C.Color

type InputState =
  { input ∷ String
  , color ∷ C.Color
  }

data InputQuery a
  = Receive C.Color a
  | Update String a

type InputDSL m = H.ComponentDSL InputState InputQuery C.Color m
type InputHTML = H.ComponentHTML InputQuery

type InputProps = Array (HP.IProp I.HTMLinput (InputQuery Unit))

type InputBijection = Iso' C.Color InputState

eval ∷ ∀ m. InputBijection → InputQuery ~> InputDSL m
eval bij = case _ of
  Receive c next → do
    st ← H.get
    unless (C.toHSLA st.color `Rec.equal` C.toHSLA c) $ H.put $ view bij c
    pure next
  Update s next → do
    H.modify (_{ input = s })
    st ← H.get
    let newColor = review bij st
    when (newColor /= st.color) do
      H.modify (_{ color = newColor })
      H.raise newColor
    pure next


_Alpha ∷ InputBijection
_Alpha = iso mkState mkColor
  where
  mkState color = { color, input: show $ _.a $ C.toHSLA color }
  mkColor { input, color } =
    let v = G.readFloat input
    in if G.isNaN v || v < 0.0 || v > 1.0 then color
       else let hsla = C.toHSLA color
            in C.hsla hsla.h hsla.s hsla.l v

_Blue ∷ InputBijection
_Blue = iso mkState mkColor
  where
  mkState color = { color, input: show $ _.b $ C.toRGBA color }
  mkColor { input, color } = M.fromMaybe color do
    v ← Int.fromString input
    guard $ v < 256 && v > -1
    let rgba = C.toRGBA color
    pure $ C.rgba rgba.r rgba.g v rgba.a

_Green ∷ InputBijection
_Green = iso mkState mkColor
  where
  mkState color = { color, input: show $ _.g $ C.toRGBA color }
  mkColor { input, color } = M.fromMaybe color do
    v ← Int.fromString input
    guard $ v < 256 && v > -1
    let rgba = C.toRGBA color
    pure $ C.rgba rgba.r v rgba.b rgba.a

_Hex ∷ InputBijection
_Hex = iso mkState mkColor
  where
  mkState color = { color, input: C.toHexString color }
  mkColor { input, color } = M.fromMaybe color do
    c ← C.fromHexString input
    let hsla = C.toHSLA c
    let oldA = _.a $ C.toHSLA color
    pure $ C.hsla hsla.h hsla.s hsla.l oldA

_Hue ∷ InputBijection
_Hue = iso mkState mkColor
  where
  mkState color = { color, input: show $ _.h $ C.toHSLA color }
  mkColor { input, color } =
    let v = G.readFloat input
    in if G.isNaN v then color
       else let hsla = C.toHSLA color
            in C.hsla v hsla.s hsla.l hsla.a

_Luminosity ∷ InputBijection
_Luminosity = iso mkState mkColor
  where
  mkState color = { color, input: show $ _.l $ C.toHSLA color }
  mkColor { input, color } =
    let v = G.readFloat input
    in if G.isNaN v || v < 0.0 || v > 1.0 then color
       else let hsla = C.toHSLA color
            in C.hsla hsla.h hsla.s v hsla.a

_Red ∷ InputBijection
_Red = iso mkState mkColor
  where
  mkState color = { color, input: show $ _.r $ C.toRGBA color }
  mkColor { input, color } = M.fromMaybe color do
    v ← Int.fromString input
    guard $ v < 256 && v > -1
    let rgba = C.toRGBA color
    pure $ C.rgba v rgba.g rgba.b rgba.a

_SaturationHSL ∷ InputBijection
_SaturationHSL = iso mkState mkColor
  where
  mkState color = { color, input: show $ _.s $ C.toHSLA color }
  mkColor { input, color } =
    let v = G.readFloat input
    in if G.isNaN v || v < 0.0 || v > 1.0 then color
       else let hsla = C.toHSLA color
            in C.hsla hsla.h v hsla.l hsla.a

_SaturationHSV ∷ InputBijection
_SaturationHSV = iso mkState mkColor
  where
  mkState color = { color, input: show $ _.s $ C.toHSVA color }
  mkColor { input, color } =
    let v = G.readFloat input
    in if G.isNaN v || v < 0.0 || v > 1.0 then color
       else let hsva = C.toHSVA color
            in C.hsva hsva.h v hsva.v hsva.a

_Value ∷ InputBijection
_Value = iso mkState mkColor
  where
  mkState color = { color, input: show $ _.v $ C.toHSVA color }
  mkColor { input, color } =
    let v = G.readFloat input
    in if G.isNaN v || v < 0.0 || v > 1.0 then color
       else let hsva = C.toHSVA color
            in C.hsva hsva.h hsva.s v hsva.a


inputComponent
  ∷ ∀ m
  . InputBijection
  → (InputState → InputHTML)
  → ColorModifier m
inputComponent bij render = HCP.proxy $ H.component
  { initialState: view bij
  , render
  , eval: eval bij
  , receiver: HE.input Receive
  }
