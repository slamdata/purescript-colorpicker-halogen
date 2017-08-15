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
  , ExistsRow
  , DragComponentView(..)
  , mkExistsRow
  , runExistsRow
  , TextComponentView(..)
  , NumberComponentView(..)
  ) where

import Prelude

import CSS (CSS)
import CSS as CSS
import Color (Color)
import Color as Color
import Control.MonadZero (guard)
import DOM.Event.Types (Event, MouseEvent, TouchEvent)
import Data.Either (Either, either, isLeft)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Halogen (ClassName)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Properties as HP
import Math (round)
import NumberInput.Halogen.Component as Num
import NumberInput.Range (Range(..))
import Unsafe.Coerce (unsafeCoerce)

type PositionUpdate = { x ∷ Number, y ∷ Number } → Dynamic Color

type Dynamic s = ColorEnv → s

type Classes = Array ClassName

-- TODO make sure all css and classes are used properly (hint: number input)
type Styles =
  { classes ∷ Classes
  , css ∷ CSS
  }

type PreNumConf = { prefix ∷ String, title ∷ String, placeholder ∷ String, range ∷ Range Number }

type PreTextConf = { prefix ∷ String, title ∷ String, placeholder ∷ String }

-- TODO add Lazy
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
    { update ∷ Number → Dynamic (Maybe Color)
    , read ∷ Dynamic Number
    , props ∷ Num.Props Number
    , view ∷ NumberComponentView
    }
  | TextComponentSpec
    { fromString ∷ String → Maybe Color
    , toString ∷ Dynamic String -- TODO remove to as in view we already ahve ColorEnv
    , view ∷ ExistsRow TextComponentView
    }
  | DragComponentSpec
    { update ∷ PositionUpdate
    , view ∷ ExistsRow DragComponentView
    }


newtype NumberComponentView = NumberComponentView
  ( ColorEnv
  → HH.HTML Void Void
  → HH.HTML Void Void)

derive instance numberComponentViewNewType :: Newtype NumberComponentView _

newtype TextComponentView r = TextComponentView
  ( ColorEnv
  → Either String String -- TODO update to Maybe String (for invlaid strings)
  → Array (HH.IProp (value :: String, onInput :: Event | r) Void)
  → HH.HTML Void Void)

newtype DragComponentView r = DragComponentView
  ( ColorEnv
  → Array (HH.IProp (onMouseDown :: MouseEvent, onTouchStart :: TouchEvent | r) Void)
  → HH.HTML Void Void)




foreign import data ExistsRow :: (# Type -> Type) -> Type

mkExistsRow :: ∀ f r. f r -> ExistsRow f
mkExistsRow = unsafeCoerce

runExistsRow :: ∀ f a. (∀ r. f r -> a) -> ExistsRow f -> a
runExistsRow = unsafeCoerce

componentDragSV ∷
  { isLight ∷ Array ClassName
  , isDark ∷ Array ClassName
  , root ∷ Array ClassName
  , selector ∷ Array ClassName
  }
  → ColorComponent
componentDragSV classes = DragComponentSpec
  { update: \{x, y} {hsv} → Color.hsv hsv.h x (1.0 - y)
  , view: mkExistsRow $ DragComponentView \{isLight, hsv, color} props ->
      HH.div
        ([ HP.classes $ classes.root <> if isLight then classes.isLight else classes.isDark
        , HCSS.style $ CSS.backgroundColor $ Color.hsl hsv.h 1.0 0.5
        ] <> props) -- TODO error here
        [ HH.div
          [ HP.classes classes.selector
          , HCSS.style do
              CSS.left $ CSS.pct (hsv.s * 100.0)
              CSS.bottom $ CSS.pct (hsv.v * 100.0)
              CSS.backgroundColor color
          ]
          []
        ]
    }


componentDragHue ∷
  { root ∷ Array ClassName
  , selector ∷ Array ClassName
  }
  → ColorComponent
componentDragHue classes = DragComponentSpec
  { update: \{y} {hsl} → Color.hsl ((1.0 - y) * 360.0) hsl.s hsl.l
  , view: mkExistsRow $ DragComponentView \{isLight, hsv, color} props ->
      HH.div
        ([ HP.classes classes.root ] <> props)
        [ HH.div
          [ HP.classes classes.selector
          , HCSS.style $ CSS.top $ CSS.pct ((1.0 - hsv.h / 360.0) * 100.0)
          ]
          []
        ]
    }

mkNumComponent
  :: (Number -> Dynamic (Maybe Color))
  -> Dynamic Number
  -> InputProps Classes
  -> PreNumConf
  -> ColorComponent
mkNumComponent update read classes conf = NumberComponentSpec
  { update
  , read
  , props:
    { title: conf.title
    , hasNumberValue: hasValRound
    , placeholder: conf.placeholder
    , range: conf.range
    , root: classes.elem
    , rootInvalid: classes.elemInvalid
    , rootLength: const []
    }
  , view: NumberComponentView \ {} input -> renderInput
      { root: classes.root
      , label: classes.label
      , prefix: conf.prefix
      , child: input
      }
  }


componentHue ∷ InputProps Classes → ColorComponent
componentHue classes = mkNumComponent
  (\n {color} → Just $ modifyHSL (_{h = n}) color)
  (\({rgb, hsv, hsl}) → roundFractionalNum hsl.h)
  classes
  confHue


componentSaturationHSL ∷ InputProps Classes → ColorComponent
componentSaturationHSL classes = mkNumComponent
  (\n {color} → Just $ modifyHSL (_{s = n / 100.0}) color)
  (\({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsl.s)
  classes
  confSaturation

componentLightness ∷ InputProps Classes → ColorComponent
componentLightness classes = mkNumComponent
  (\n {color} → Just $ modifyHSL (_{l = n / 100.0}) color)
  (\({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsl.l)
  classes
  confLightness

componentSaturationHSV ∷ InputProps Classes → ColorComponent
componentSaturationHSV classes = mkNumComponent
  (\n {color} → Just $ modifyHSV (_{s = n / 100.0}) color)
  (\({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsv.s)
  classes
  confSaturation

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
componentValue classes = mkNumComponent
  (\n {color} → Just $ modifyHSV (_{v = n / 100.0}) color)
  (\({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * hsv.v)
  classes
  confValue

componentRed ∷ InputProps Classes → ColorComponent
componentRed classes = mkNumComponent
  (\n {color} → Just $ modifyRGB (_{r = asInt n}) color)
  (\({rgb, hsv, hsl}) → roundNum $ toNumber rgb.r)
  classes
  confRed

componentGreen ∷ InputProps Classes → ColorComponent
componentGreen classes = mkNumComponent
  (\n {color} → Just $ modifyRGB (_{g = asInt n}) color)
  (\({rgb, hsv, hsl}) → roundNum $ toNumber rgb.g)
  classes
  confGreen

componentBlue ∷ InputProps Classes → ColorComponent
componentBlue classes = mkNumComponent
  (\n {color} → Just $ modifyRGB (_{b = asInt n}) color)
  (\({rgb, hsv, hsl}) → roundNum $ toNumber rgb.b)
  classes
  confBlue

componentHEX ∷ InputProps Classes → ColorComponent
componentHEX classes = TextComponentSpec
  { fromString: \str → Color.fromHexString $ "#" <> str
  , toString: \{color} → String.toUpper $ String.drop 1 $ Color.toHexString color
  , view: mkExistsRow $ TextComponentView \env val props -> renderInput
      { root: classes.root
      , label: classes.label
      , prefix: "#"
      , child: HH.input $
          [ HP.type_ HP.InputText
          , HP.classes
            $  classes.elem
            <> (guard (isLeft val) *> (classes.elemInvalid))
          , HP.title "Hex"
          , HP.value $ either id id val
          , HP.placeholder "Hex"
          ] <> props
      }
  }

renderInput :: ∀ i p.
  { child :: HH.HTML i p
  , prefix :: String
  , label :: Classes
  , root :: Classes
  }
  -> HH.HTML i p
renderInput {root, label, prefix, child} =
  HH.label
    [ HP.classes root]
    [ HH.span [HP.classes label] [HH.text prefix]
    , child
    ]

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
