module ColorPicker.Halogen.Layout where


import ColorPicker.Halogen.ColorComponents (ColorComponent)
import Halogen (ClassName)

data Layout
  = Group (Array ClassName) (Array Layout)
  | Component ColorComponent
  | Stage
  | Actions
