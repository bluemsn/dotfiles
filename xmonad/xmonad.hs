-- Imports
import XMonad
import XMonad.Hooks.DynamicLog

-- The main function
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)
myConfig = defaultConfig { modMask = mod4Mask
                         , terminal = "urxvt"
                         , borderWidth = 2 }
