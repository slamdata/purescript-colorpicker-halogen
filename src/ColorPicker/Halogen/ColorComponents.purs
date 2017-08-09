module ColorPicker.Halogen.ColorComponents
  ( ColorComponent(..)
  , PreNumConf
  , PreTextConf
  , RecordHSLA
  , RecordHSVA
  , RecordRGBA
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
  ) where

import Prelude

import Color (Color)
import Color as Color
import Data.Int (floor, toNumber)
import Data.String as String
import Data.Maybe (Maybe(..))
import Math (round)
import NumberInput.Halogen.Component as Num
import NumberInput.Range (Range(..))


type PreNumConf a = { prefix ∷ String, title ∷ String, placeholder ∷ String, range ∷ Range a }
type PreTextConf = { prefix ∷ String, title ∷ String, placeholder ∷ String }

data ColorComponent
  = NumberComponentSpec
    { hasNumVal ∷ Num.HasNumberInputVal Number
    , update ∷ Number → Color → Maybe Color
    , key ∷ String
    , read ∷
      { hsl ∷ RecordHSLA
      , hsv ∷ RecordHSVA
      , rgb ∷ RecordRGBA
      } → Number
    , config ∷ PreNumConf Number
    }
  | TextComponentSpec
    { fromString ∷ String → Maybe Color
    , toString ∷ Color → String
    , key ∷ String
    , config ∷ PreTextConf
    }


componentHue ∷ ColorComponent
componentHue = NumberComponentSpec
  { hasNumVal: hasValRound
  , key: "Hue"
  , update: \n color → Just $ modifyHSL (_{h = n}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum hsl.h
  , config: confHue
  }


componentSaturationHSL ∷ ColorComponent
componentSaturationHSL = NumberComponentSpec
  { hasNumVal: hasValRound
  , key: "SaturationHSL"
  , update: \n color → Just $ modifyHSL (_{s = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsl.s
  , config: confSaturation
  }

componentLightness ∷ ColorComponent
componentLightness = NumberComponentSpec
  { hasNumVal: hasValRound
  , key: "Lightness"
  , update: \n color → Just $ modifyHSL (_{l = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsl.l
  , config: confLightness
  }

componentSaturationHSV ∷ ColorComponent
componentSaturationHSV = NumberComponentSpec
  { hasNumVal: hasValRound
  , key: "SaturationHSV"
  , update: \n color → Just $ modifyHSL (_{s = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsv.s
  , config: confSaturation
  }

componentValue ∷ ColorComponent
componentValue = NumberComponentSpec
  { hasNumVal: hasValRound
  , key: "Value"
  , update: \n color → Just $ modifyHSL (_{l = n / 100.0}) color
  , read: \({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsv.v
  , config: confValue
  }

componentRed ∷ ColorComponent
componentRed = NumberComponentSpec
  { hasNumVal: hasvalCail
  , key: "Red"
  , update: \n color → Just $ modifyRGB (_{r = asInt n}) color
  , read: \({rgb, hsv, hsl}) → roundNum $ toNumber rgb.r
  , config: confRed
  }

componentGreen ∷ ColorComponent
componentGreen = NumberComponentSpec
  { hasNumVal: hasvalCail
  , key: "Green"
  , update: \n color → Just $ modifyRGB (_{g = asInt n}) color
  , read: \({rgb, hsv, hsl}) → roundNum $ toNumber rgb.g
  , config: confGreen
  }

componentBlue ∷ ColorComponent
componentBlue = NumberComponentSpec
  { hasNumVal: hasvalCail
  , key: "Blue"
  , update: \n color → Just $ modifyRGB (_{b = asInt n}) color
  , read: \({rgb, hsv, hsl}) → roundNum $ toNumber rgb.b
  , config: confBlue
  }

componentHEX ∷ ColorComponent
componentHEX = TextComponentSpec
  { fromString: \str → Color.fromHexString $ "#" <> str
  , toString: \color → String.toUpper $ String.drop 1 $ Color.toHexString color
  , key: "HEX"
  , config:
      { prefix: "#"
      , title: "Hex"
      , placeholder: "HEX"
      }
  }

componentSL ∷ Array ColorComponent
componentSL = [componentSaturationHSL, componentLightness]

componentSV ∷ Array ColorComponent
componentSV = [componentSaturationHSV, componentValue]

componentRGB ∷ Array ColorComponent
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
modifyHSL f c = case f (Color.toHSLA c) of {h, s, l, a} → Color.hsla h s l a

modifyHSV ∷ (RecordHSVA → RecordHSVA) → Color → Color
modifyHSV f c = case f (Color.toHSVA c) of {h, s, v, a} → Color.hsva h s v a

modifyRGB ∷ (RecordRGBA → RecordRGBA) → Color → Color
modifyRGB f c = case f (Color.toRGBA c) of {r, g, b, a} → Color.rgba r g b a
