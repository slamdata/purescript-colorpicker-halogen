module ColorPicker.Halogen.Layout where


import ColorPicker.Halogen.ColorComponents (ColorComponent)
import Halogen (ClassName)

data Layout
  = Group (Array ClassName) (Array Layout)
  | Component ColorComponent
  -- TODO remove this Stage and Actions variants they could be expressed using some ButtonComponents
  | Stage
  | Actions
