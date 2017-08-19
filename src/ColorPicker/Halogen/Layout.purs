module ColorPicker.Halogen.Layout
  ( Layout(..)
  , ChildLayout(..)
  , PickerComponent(..)

  , NumberComponentView
  , TextComponentView
  , DragComponentView
  , ActionComponentView

  , InputTextValue
  , isValid
  , isInvalid

  , PositionUpdate
  , Dynamic
  , PreNumConf
  , InputProps
  , Classes
  , ValueHistory
  , mapValueHistory

  , LazyColor
  , RecordHSLA
  , RecordHSVA
  , RecordRGBA
  , mkLazyColor


  , componentHue
  , componentSaturationHSL
  , componentLightness
  , componentSaturationHSV
  , componentValue
  , componentRed
  , componentGreen
  , componentBlue
  , componentHEX
  , componentDragHue
  , componentDragSV
  , componentPreview
  , componentHistory
  , componentSet
  ) where

import Prelude

import CSS as CSS
import Color (Color)
import Color as Color
import Control.MonadZero (guard)
import DOM.Event.Types (FocusEvent, MouseEvent, TouchEvent)
import Data.Array (head, take)
import Data.Int (floor, toNumber)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (round)
import NumberInput.Halogen.Component as Num
import NumberInput.Range (Range(..))

data Layout = Root (Array H.ClassName) (Array ChildLayout)

data ChildLayout
  = Group (Array H.ClassName) (Array ChildLayout)
  | Component PickerComponent


type ValueHistory a =  { old ∷ Array a, current ∷ a }

type InputTextValue = { value ∷ String, isValid ∷ Boolean }

type PositionUpdate = { x ∷ Number, y ∷ Number } → Dynamic Color

type Dynamic s = LazyColor → s

type Classes = Array H.ClassName

type PreNumConf = { prefix ∷ String, title ∷ String, placeholder ∷ String, range ∷ Range Number }

type LazyColor =
  { color ∷ Color
  , hsl ∷ Lazy RecordHSLA
  , hsv ∷ Lazy RecordHSVA
  , rgb ∷ Lazy RecordRGBA
  }

type InputProps c =
  { root ∷ c
  , label ∷ c
  , elem ∷ c
  , elemInvalid ∷ c
  }


data PickerComponent
  = NumberComponentSpec
    { update ∷ Number → Dynamic (Maybe Color)
    , read ∷ Dynamic Number
    , props ∷ Num.Props Number
    , view ∷ NumberComponentView
    }
  | TextComponentSpec
    { fromString ∷ String → Maybe Color
    , view ∷ TextComponentView
    }
  | DragComponentSpec
    { update ∷ PositionUpdate
    , view ∷ DragComponentView
    }
  | ActionComponentSpec ActionComponentView

type NumberComponentView =
  ∀ p i
  . { color ∷ LazyColor
    , input ∷ HH.HTML p i
    }
  → Array (HH.HTML p i)

type TextComponentView =
  ∀ p i
  . { color ∷ LazyColor
    , value ∷ Maybe InputTextValue
    , onValueInput ∷ String → i
    , onBlur ∷ FocusEvent → i
    }
  → Array (HH.HTML p i)

type DragComponentView =
  ∀ p i
  . { color∷ LazyColor
    , onMouseDown ∷ MouseEvent → i
    , onTouchStart ∷ TouchEvent → i
    }
  → Array (HH.HTML p i)

type ActionComponentView =
  ∀ p i
  . { color ∷ ValueHistory LazyColor
    , setColor ∷ Color → i
    , commit ∷ i
    }
  → Array (HH.HTML p i)

mapValueHistory ∷ ∀ a b. (a → b) → ValueHistory a → ValueHistory b
mapValueHistory f { current, old } = { current: f current, old: map f old }

mkLazyColor ∷ Color → LazyColor
mkLazyColor color =
  { color
  , hsl: defer \_ → Color.toHSLA color
  , hsv: defer \_ → Color.toHSVA color
  , rgb: defer \_ → Color.toRGBA color
  }


componentPreview ∷ Array H.ClassName → PickerComponent
componentPreview classes = ActionComponentSpec \{ color , setColor } → pure $
  HH.div
    [ HP.classes $ classes
    , HP.title "Current value"
    , HCSS.style $ CSS.backgroundColor color.current.color
    ] []

componentHistory ∷ Int → Array H.ClassName → PickerComponent
componentHistory historySize classes = ActionComponentSpec \{ color , setColor } →
  take historySize color.old <#> \c →
    HH.div
      [ HP.tabIndex 0
      , HP.classes $ classes
      , HP.title "Old value"
      , HE.onClick $ (\_ → Just $ setColor c.color)
      , HCSS.style $ CSS.backgroundColor c.color
      ] []


componentSet ∷ Array H.ClassName → PickerComponent
componentSet classes = ActionComponentSpec \{ color: {current, old}, commit } → pure $
  HH.button
    [ HP.classes classes
    , HE.onClick $ const $ Just commit
    , HP.disabled $ Just current.color == (map _.color $ head old)
    ]
    [ HH.text "Set" ]

componentDragSV ∷
  { isLight ∷ Array H.ClassName
  , isDark ∷ Array H.ClassName
  , root ∷ Array H.ClassName
  , selector ∷ Array H.ClassName
  }
  → PickerComponent
componentDragSV classes = DragComponentSpec
  { update: \{x, y} → modifyHSV _{ s = x, v = 1.0 - y}
  , view: \{color: {hsv, color}, onMouseDown, onTouchStart} → pure $
      HH.div
        [ HP.classes $ classes.root <> if Color.isLight color then classes.isLight else classes.isDark
        , HCSS.style $ CSS.backgroundColor $ Color.hsl (force hsv).h 1.0 0.5
        , HE.onTouchStart $ onTouchStart >>> Just
        , HE.onMouseDown $ onMouseDown >>> Just
        ]
        [ HH.div
          [ HP.classes classes.selector
          , HCSS.style do
              CSS.left $ CSS.pct ((force hsv).s * 100.0)
              CSS.bottom $ CSS.pct ((force hsv).v * 100.0)
              CSS.backgroundColor color
          ]
          []
        ]
    }


componentDragHue ∷
  { root ∷ Array H.ClassName
  , selector ∷ Array H.ClassName
  }
  → PickerComponent
componentDragHue classes = DragComponentSpec
  { update: \{y} → modifyHSL _{ h = (1.0 - y) * 360.0 }
  , view: \{color: {hsv, color}, onMouseDown, onTouchStart} → pure $
      HH.div
        [ HP.classes classes.root
        , HE.onTouchStart $ onTouchStart >>> Just
        , HE.onMouseDown $ onMouseDown >>> Just
        ]
        [ HH.div
          [ HP.classes classes.selector
          , HCSS.style $ CSS.top $ CSS.pct ((1.0 - (force hsv).h / 360.0) * 100.0)
          ]
          []
        ]
    }

mkNumComponent
  ∷ (Number → Dynamic (Maybe Color))
  → Dynamic Number
  → InputProps Classes
  → PreNumConf
  → PickerComponent
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
  , view: \ {input} → pure $ renderInput
      { root: classes.root
      , label: classes.label
      , prefix: conf.prefix
      , child: input
      }
  }


componentHue ∷ InputProps Classes → PickerComponent
componentHue classes = mkNumComponent
  (\n → Just <<< modifyHSL (_{h = n}))
  (\({rgb, hsv, hsl}) → roundFractionalNum (force hsl).h)
  classes
  confHue


componentSaturationHSL ∷ InputProps Classes → PickerComponent
componentSaturationHSL classes = mkNumComponent
  (\n → Just <<< modifyHSL (_{s = n / 100.0}))
  (\({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * (force hsl).s)
  classes
  confSaturation

componentLightness ∷ InputProps Classes → PickerComponent
componentLightness classes = mkNumComponent
  (\n → Just <<< modifyHSL (_{l = n / 100.0}))
  (\({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * (force hsl).l)
  classes
  confLightness

componentSaturationHSV ∷ InputProps Classes → PickerComponent
componentSaturationHSV classes = mkNumComponent
  (\n → Just <<< modifyHSV (_{s = n / 100.0}))
  (\({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * (force hsv).s)
  classes
  confSaturation


componentValue ∷ InputProps Classes → PickerComponent
componentValue classes = mkNumComponent
  (\n → Just <<< modifyHSV (_{v = n / 100.0}))
  (\({rgb, hsv, hsl}) → roundFractionalNum $ 100.0 * (force hsv).v)
  classes
  confValue

componentRed ∷ InputProps Classes → PickerComponent
componentRed classes = mkNumComponent
  (\n → Just <<< modifyRGB (_{r = asInt n}))
  (\({rgb, hsv, hsl}) → roundNum $ toNumber (force rgb).r)
  classes
  confRed

componentGreen ∷ InputProps Classes → PickerComponent
componentGreen classes = mkNumComponent
  (\n → Just <<< modifyRGB (_{g = asInt n}))
  (\({rgb, hsv, hsl}) → roundNum $ toNumber (force rgb).g)
  classes
  confGreen

componentBlue ∷ InputProps Classes → PickerComponent
componentBlue classes = mkNumComponent
  (\n → Just <<< modifyRGB (_{b = asInt n}))
  (\({rgb, hsv, hsl}) → roundNum $ toNumber (force rgb).b)
  classes
  confBlue

componentHEX ∷ InputProps Classes → PickerComponent
componentHEX classes = TextComponentSpec
  { fromString: \str → Color.fromHexString $ "#" <> str
  , view: \{color, value, onValueInput, onBlur} → pure $
      renderInput
        { root: classes.root
        , label: classes.label
        , prefix: "#"
        , child: HH.input $
            [ HP.type_ HP.InputText
            , HP.classes
              $  classes.elem
              <> (guard (isInvalid value) *> (classes.elemInvalid))
            , HP.title "Hex"
            , HP.value $ maybe' (\_ → toString color) _.value value
            , HP.placeholder "Hex"
            , HE.onValueInput $ onValueInput >>> Just
            , HE.onBlur $ onBlur >>> Just
            ]
        }
  }
  where
  toString =  \{color} → String.toUpper $ String.drop 1 $ Color.toHexString color


isValid ∷ Maybe InputTextValue → Boolean
isValid = maybe true _.isValid

isInvalid ∷ Maybe InputTextValue → Boolean
isInvalid = not isValid

renderInput ∷ ∀ i p.
  { child ∷ HH.HTML i p
  , prefix ∷ String
  , label ∷ Classes
  , root ∷ Classes
  }
  → HH.HTML i p
renderInput {root, label, prefix, child} =
  HH.label
    [ HP.classes root]
    [ HH.span [HP.classes label] [HH.text prefix]
    , child
    ]


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

modifyHSL ∷ (RecordHSLA → RecordHSLA) → Dynamic Color
modifyHSL f { hsl } = let {h, s, l, a} = f (force hsl) in Color.hsla h s l a

modifyHSV ∷ (RecordHSVA → RecordHSVA) → Dynamic Color
modifyHSV f { hsv } = let {h, s, v, a} = f (force hsv) in Color.hsva h s v a

modifyRGB ∷ (RecordRGBA → RecordRGBA) → Dynamic Color
modifyRGB f { rgb } = let {r, g, b, a} = f (force rgb) in Color.rgba r g b a