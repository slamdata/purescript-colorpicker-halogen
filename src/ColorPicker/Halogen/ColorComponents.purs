module ColorPicker.Halogen.ColorComponents
  ( ColorComponent(..)
  , PreNumConf
  , PreTextConf
  , InputClasses
  , RecordHSLA
  , RecordHSVA
  , RecordRGBA
  , StyleProps
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

type PositionUpdate = { x ∷ Number, y ∷ Number } → Color → Color

type StyleProps =
  { classes ∷ Array ClassName
  , css ∷ CSS
  }

type PreNumConf a = { prefix ∷ String, title ∷ String, placeholder ∷ String, range ∷ Range a }

type PreTextConf = { prefix ∷ String, title ∷ String, placeholder ∷ String }

type ColorEnv =
  { hsl ∷ RecordHSLA
  , hsv ∷ RecordHSVA
  , rgb ∷ RecordRGBA
  , isLight ∷ Boolean
  , color ∷ Color
  }

type InputClasses =
  { root ∷ Array ClassName
  , label ∷ Array ClassName
  , elem ∷ Array ClassName
  , elemInvalid ∷ Array ClassName
  }

--TODO remove key from spec
data ColorComponent
  = NumberComponentSpec
    { hasNumVal ∷ Num.HasNumberInputVal Number
    , update ∷ Number → Color → Maybe Color
    , key ∷ String
    , read ∷ ColorEnv → Number
    , config ∷ PreNumConf Number
    , classes ∷ InputClasses
    }
  | TextComponentSpec
    { fromString ∷ String → Maybe Color
    , toString ∷ Color → String
    , key ∷ String
    , config ∷ PreTextConf
    , classes ∷ InputClasses
    }
  | DragComponentSpec
    { root ∷ ColorEnv → StyleProps
    , selector ∷ ColorEnv → StyleProps
    , update ∷ { x ∷ Number, y ∷ Number } → Color → Color
    }


componentDragSV ∷
  { isLight ∷ Array ClassName
  , isDark ∷ Array ClassName
  , root ∷ Array ClassName
  , selector ∷ Array ClassName
  }
  → ColorComponent
componentDragSV classes = DragComponentSpec
  { update : \{x, y} color →
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
  { update : \{y} color →
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

componentHue ∷ InputClasses → ColorComponent
componentHue classes = NumberComponentSpec
  { classes
  , hasNumVal: hasValRound
  , key: "Hue"
  , update: \n color → Just $ modifyHSL (_{h = n}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum hsl.h
  , config: confHue
  }


componentSaturationHSL ∷ InputClasses → ColorComponent
componentSaturationHSL classes = NumberComponentSpec
  { classes
  , hasNumVal: hasValRound
  , key: "SaturationHSL"
  , update: \n color → Just $ modifyHSL (_{s = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsl.s
  , config: confSaturation
  }

componentLightness ∷ InputClasses → ColorComponent
componentLightness classes = NumberComponentSpec
  { classes
  , hasNumVal: hasValRound
  , key: "Lightness"
  , update: \n color → Just $ modifyHSL (_{l = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsl.l
  , config: confLightness
  }

componentSaturationHSV ∷ InputClasses → ColorComponent
componentSaturationHSV classes = NumberComponentSpec
  { classes
  , hasNumVal: hasValRound
  , key: "SaturationHSV"
  , update: \n color → Just $ modifyHSV (_{s = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsv.s
  , config: confSaturation
  }

componentValue ∷ InputClasses → ColorComponent
componentValue classes = NumberComponentSpec
  { classes
  , hasNumVal: hasValRound
  , key: "Value"
  , update: \n color → Just $ modifyHSV (_{v = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsv.v
  , config: confValue
  }

componentRed ∷ InputClasses → ColorComponent
componentRed classes = NumberComponentSpec
  { classes
  , hasNumVal: hasvalCail
  , key: "Red"
  , update: \n color → Just $ modifyRGB (_{r = asInt n}) color
  , read: \({rgb, hsv, hsl}) → roundNum $ toNumber rgb.r
  , config: confRed
  }

componentGreen ∷ InputClasses → ColorComponent
componentGreen classes = NumberComponentSpec
  { classes
  , hasNumVal: hasvalCail
  , key: "Green"
  , update: \n color → Just $ modifyRGB (_{g = asInt n}) color
  , read: \({rgb, hsv, hsl}) → roundNum $ toNumber rgb.g
  , config: confGreen
  }

componentBlue ∷ InputClasses → ColorComponent
componentBlue classes = NumberComponentSpec
  { classes
  , hasNumVal: hasvalCail
  , key: "Blue"
  , update: \n color → Just $ modifyRGB (_{b = asInt n}) color
  , read: \({rgb, hsv, hsl}) → roundNum $ toNumber rgb.b
  , config: confBlue
  }

componentHEX ∷ InputClasses → ColorComponent
componentHEX classes = TextComponentSpec
  { classes
  , fromString: \str → Color.fromHexString $ "#" <> str
  , toString: \color → String.toUpper $ String.drop 1 $ Color.toHexString color
  , key: "HEX"
  , config:
      { prefix: "#"
      , title: "Hex"
      , placeholder: "HEX"
      }
  }

componentSL ∷ Array (InputClasses → ColorComponent)
componentSL = [componentSaturationHSL, componentLightness]

componentSV ∷ Array (InputClasses → ColorComponent)
componentSV = [componentSaturationHSV, componentValue]

componentRGB ∷ Array (InputClasses → ColorComponent)
componentRGB = [componentRed, componentGreen, componentBlue]


-- Internal helpers


confRed ∷ PreNumConf Number
confRed =
  { title: "Red"
  , placeholder: "R"
  , prefix: "R"
  , range: MinMax 0.0 256.0
  }

confGreen ∷ PreNumConf Number
confGreen =
  { title: "Green"
  , placeholder: "G"
  , prefix: "G"
  , range: MinMax 0.0 256.0
  }

confBlue ∷ PreNumConf Number
confBlue =
  { title: "Blue"
  , placeholder: "B"
  , prefix: "B"
  , range: MinMax 0.0 256.0
  }

confHue ∷ PreNumConf Number
confHue =
  { title: "Hue"
  , placeholder: "H"
  , prefix: "H"
  , range: MinMax 0.0 360.0
  }
confSaturation ∷ PreNumConf Number
confSaturation =
  { title: "Saturation"
  , placeholder: "S"
  , prefix: "S"
  , range: MinMax 0.0 100.0
  }
confLightness ∷ PreNumConf Number
confLightness =
  { title: "Lightness"
  , placeholder: "L"
  , prefix: "L"
  , range: MinMax 0.0 100.0
  }

confValue ∷ PreNumConf Number
confValue =
  { title: "Value"
  , placeholder: "V"
  , prefix: "V"
  , range: MinMax 0.0 100.0
  }


asInt ∷ Number → Int
asInt = floor

hasValRound ∷ Num.HasNumberInputVal Number
hasValRound = Num.numberHasNumberInputVal
  {fromString = Num.numberHasNumberInputVal.fromString >>> map roundFractionalNum}

hasvalCail ∷ Num.HasNumberInputVal Number
hasvalCail = Num.numberHasNumberInputVal
  {fromString = Num.numberHasNumberInputVal.fromString >>> map roundNum}

roundFractionalNum ∷ Number → Number
roundFractionalNum n = roundNum (n * scalar) / scalar
  where
  scalar = 100.0

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
