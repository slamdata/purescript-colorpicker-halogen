module ColorPicker.Halogen.ColorComponents
  ( ColorComponent(..)
  , Dynamic
  , Styles
  , PreNumConf
  , PreTextConf
  , InputProps
  , Classes
  , RecordHSLA
  , RecordHSVA
  , RecordRGBA
  , PositionUpdate
  , ColorEnv
  , componentHue
  , componentSaturationHSL
  , componentLightness
  , componentSaturationHSV
  , componentValue
  , componentRed
  , componentGreen
  , componentBlue
  , componentHEX
  , componentSL
  , componentSV
  , componentRGB
  , componentDragHue
  , componentDragSV
  , mapInputProps
  , toDynamicStyles
  ) where

import Prelude

import CSS (CSS)
import CSS as CSS
import Color (Color)
import Color as Color
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.String as String
import Halogen (ClassName)
import Math (round)
import NumberInput.Halogen.Component as Num
import NumberInput.Range (Range(..))

type PositionUpdate = { x ∷ Number, y ∷ Number } → Dynamic Color

type Dynamic s = ColorEnv → s

type Classes = Array ClassName

type Styles =
  { classes ∷ Classes
  , css ∷ CSS
  }

type PreNumConf = { prefix ∷ String, title ∷ String, placeholder ∷ String, range ∷ Range Number }

type PreTextConf = { prefix ∷ String, title ∷ String, placeholder ∷ String }

type ColorEnv =
  { hsl ∷ RecordHSLA
  , hsv ∷ RecordHSVA
  , rgb ∷ RecordRGBA
  , isLight ∷ Boolean
  , color ∷ Color
  }

type InputProps c =
  { root ∷ c
  , label ∷ c
  , elem ∷ c
  , elemInvalid ∷ c
  }


data ColorComponent
  = NumberComponentSpec
    { hasNumVal ∷ Num.HasNumberInputValue Number
    , update ∷ Number → Dynamic (Maybe Color)
    , read ∷ Dynamic Number
    , config ∷ PreNumConf
    , styles ∷ InputProps (Dynamic Styles)
    }
  | TextComponentSpec
    { fromString ∷ String → Maybe Color
    , toString ∷ Dynamic String
    , config ∷ PreTextConf
    , styles ∷ InputProps (Dynamic Styles)
    }
  | DragComponentSpec
    { root ∷ Dynamic Styles
    , selector ∷ Dynamic Styles
    , update ∷ PositionUpdate
    }


componentDragSV ∷
  { isLight ∷ Array ClassName
  , isDark ∷ Array ClassName
  , root ∷ Array ClassName
  , selector ∷ Array ClassName
  }
  → ColorComponent
componentDragSV classes = DragComponentSpec
  { update : \{x, y} {color} →
      let hsv = Color.toHSVA color
      in Color.hsv hsv.h x (1.0 - y)
  , root: \{isLight, hsv} →
    { classes: classes.root <> if isLight then classes.isLight else classes.isDark
    , css: CSS.backgroundColor $ Color.hsl hsv.h 1.0 0.5
    }
  , selector: \{hsv, color} →
    { classes: classes.selector
    , css: do
        CSS.left $ CSS.pct (hsv.s * 100.0)
        CSS.bottom $ CSS.pct (hsv.v * 100.0)
        CSS.backgroundColor color
    }
  }

componentDragHue ∷
  { root ∷ Array ClassName
  , selector ∷ Array ClassName
  }
  → ColorComponent
componentDragHue classes = DragComponentSpec
  { update : \{y} {color} →
      let hsl = Color.toHSLA $ color
      in (Color.hsl ((1.0 - y) * 360.0) hsl.s hsl.l)
  , root: \_ →
    { classes: classes.root
    , css: pure unit
    }
  , selector: \{hsv} →
    { classes: classes.selector
    , css: CSS.top $ CSS.pct ((1.0 - hsv.h / 360.0) * 100.0)
    }
  }

componentHue ∷ InputProps Classes → ColorComponent
componentHue classes = NumberComponentSpec
  { styles: toDynamicStyles classes
  , hasNumVal: hasValRound
  , update: \n {color} → Just $ modifyHSL (_{h = n}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum hsl.h
  , config: confHue
  }


componentSaturationHSL ∷ InputProps Classes → ColorComponent
componentSaturationHSL classes = NumberComponentSpec
  { styles: toDynamicStyles classes
  , hasNumVal: hasValRound
  , update: \n {color} → Just $ modifyHSL (_{s = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsl.s
  , config: confSaturation
  }

componentLightness ∷ InputProps Classes → ColorComponent
componentLightness classes = NumberComponentSpec
  { styles: toDynamicStyles classes
  , hasNumVal: hasValRound
  , update: \n {color} → Just $ modifyHSL (_{l = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsl.l
  , config: confLightness
  }

componentSaturationHSV ∷ InputProps Classes → ColorComponent
componentSaturationHSV classes = NumberComponentSpec
  { styles: toDynamicStyles classes
  , hasNumVal: hasValRound
  , update: \n {color} → Just $ modifyHSV (_{s = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsv.s
  , config: confSaturation
  }

mapInputProps :: ∀ a b. (a -> b) -> InputProps a -> InputProps b
mapInputProps f { root, label, elem, elemInvalid } =
  { root: f root
  , label: f label
  , elem: f elem
  , elemInvalid: f elemInvalid
  }

toDynamicStyles :: InputProps Classes -> InputProps (Dynamic Styles)
toDynamicStyles = mapInputProps $ { classes: _, css: pure unit } >>> const

componentValue ∷ InputProps Classes → ColorComponent
componentValue classes = NumberComponentSpec
  { styles: toDynamicStyles classes
  , hasNumVal: hasValRound
  , update: \n {color} → Just $ modifyHSV (_{v = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsv.v
  , config: confValue
  }

componentRed ∷ InputProps Classes → ColorComponent
componentRed classes = NumberComponentSpec
  { styles: toDynamicStyles classes
  , hasNumVal: hasValCail
  , update: \n {color} → Just $ modifyRGB (_{r = asInt n}) color
  , read: \({rgb, hsv, hsl}) → roundNum $ toNumber rgb.r
  , config: confRed
  }

componentGreen ∷ InputProps Classes → ColorComponent
componentGreen classes = NumberComponentSpec
  { styles: toDynamicStyles classes
  , hasNumVal: hasValCail
  , update: \n {color} → Just $ modifyRGB (_{g = asInt n}) color
  , read: \({rgb, hsv, hsl}) → roundNum $ toNumber rgb.g
  , config: confGreen
  }

componentBlue ∷ InputProps Classes → ColorComponent
componentBlue classes = NumberComponentSpec
  { styles: toDynamicStyles classes
  , hasNumVal: hasValCail
  , update: \n {color} → Just $ modifyRGB (_{b = asInt n}) color
  , read: \({rgb, hsv, hsl}) → roundNum $ toNumber rgb.b
  , config: confBlue
  }

componentHEX ∷ InputProps Classes → ColorComponent
componentHEX classes = TextComponentSpec
  { styles: toDynamicStyles classes
  , fromString: \str → Color.fromHexString $ "#" <> str
  , toString: \{color} → String.toUpper $ String.drop 1 $ Color.toHexString color
  , config:
      { prefix: "#"
      , title: "Hex"
      , placeholder: "HEX"
      }
  }

componentSL ∷ Array (InputProps Classes → ColorComponent)
componentSL = [componentSaturationHSL, componentLightness]

componentSV ∷ Array (InputProps Classes → ColorComponent)
componentSV = [componentSaturationHSV, componentValue]

componentRGB ∷ Array (InputProps Classes → ColorComponent)
componentRGB = [componentRed, componentGreen, componentBlue]


-- Internal helpers


confRed ∷ PreNumConf
confRed =
  { title: "Red"
  , placeholder: "R"
  , prefix: "R"
  , range: MinMax 0.0 256.0
  }

confGreen ∷ PreNumConf
confGreen =
  { title: "Green"
  , placeholder: "G"
  , prefix: "G"
  , range: MinMax 0.0 256.0
  }

confBlue ∷ PreNumConf
confBlue =
  { title: "Blue"
  , placeholder: "B"
  , prefix: "B"
  , range: MinMax 0.0 256.0
  }

confHue ∷ PreNumConf
confHue =
  { title: "Hue"
  , placeholder: "H"
  , prefix: "H"
  , range: MinMax 0.0 360.0
  }
confSaturation ∷ PreNumConf
confSaturation =
  { title: "Saturation"
  , placeholder: "S"
  , prefix: "S"
  , range: MinMax 0.0 100.0
  }
confLightness ∷ PreNumConf
confLightness =
  { title: "Lightness"
  , placeholder: "L"
  , prefix: "L"
  , range: MinMax 0.0 100.0
  }

confValue ∷ PreNumConf
confValue =
  { title: "Value"
  , placeholder: "V"
  , prefix: "V"
  , range: MinMax 0.0 100.0
  }


hasValRound ∷ Num.HasNumberInputValue Number
hasValRound = Num.numberHasNumberInputValue
  {fromString = Num.numberHasNumberInputValue.fromString >>> map roundFractionalNum}

hasValCail ∷ Num.HasNumberInputValue Number
hasValCail = Num.numberHasNumberInputValue
  {fromString = Num.numberHasNumberInputValue.fromString >>> map roundNum}

roundFractionalNum ∷ Number → Number
roundFractionalNum n = roundNum (n * scalar) / scalar
  where
  scalar = 100.0

asInt ∷ Number → Int
asInt = floor

roundNum ∷ Number → Number
roundNum = round

type RecordHSLA = { h ∷ Number, s ∷ Number, l ∷ Number, a ∷ Number }
type RecordHSVA = { h ∷ Number, s ∷ Number, v ∷ Number, a ∷ Number }
type RecordRGBA = { r ∷ Int, g ∷ Int, b ∷ Int, a ∷ Number }

modifyHSL ∷ (RecordHSLA → RecordHSLA) → Color → Color
modifyHSL f c = let {h, s, l, a} = f (Color.toHSLA c) in Color.hsla h s l a

modifyHSV ∷ (RecordHSVA → RecordHSVA) → Color → Color
modifyHSV f c = let {h, s, v, a} = f (Color.toHSVA c) in Color.hsva h s v a

modifyRGB ∷ (RecordRGBA → RecordRGBA) → Color → Color
modifyRGB f c = let {r, g, b, a} = f (Color.toRGBA c) in Color.rgba r g b a
