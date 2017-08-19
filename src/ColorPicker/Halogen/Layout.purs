module ColorPicker.Halogen.Layout where


import ColorPicker.Halogen.ColorComponents (ColorComponent)
import Halogen (ClassName)

data Layout = Root (Array ClassName) (Array ChildLayout)

data ChildLayout
  = Group (Array ClassName) (Array ChildLayout)
  | Component ColorComponent
