-- Imports
import XMonad
import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.ManageDocks
--import XMonad.Util.EZConfig(additionalKeys)
--import System.IO

myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#fe8019" "" . wrap "[" "]"
                , ppTitle = xmobarColor "#fe8019" "" }
statusbarToggleKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myConfig = defaultConfig { modMask = mod4Mask
                         --, manageHook = manageDocks <+> manageHook defaultConfig
                         --, layoutHook = avoidStruts $ layoutHook defaultConfig
                         , terminal = "urxvt"
                         , workspaces = ["1","2","3","4","5","6","7","8","9"]
                         , borderWidth = 2 }
                         --`additionalKeys` [ ((modMask, xK_c), kill) ]

-- The main function
main = xmonad =<< statusBar myBar myPP statusbarToggleKey myConfig
