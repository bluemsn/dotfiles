-- Imports
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "[" "]" }
statusbarToggleKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myConfig = defaultConfig { modMask = mod4Mask
                         --, manageHook = manageDocks <+> manageHook defaultConfig
                         --, layoutHook = avoidStruts $ layoutHook defaultConfig
                         , terminal = "urxvt"
                         , workspaces = ["1:main","2:dev","3:comm","4:media","5:misc","6","7","8","9"]
                         , borderWidth = 2 }
                         --`additionalKeys` [ ((modMask, xK_c), kill) ]

-- The main function
main = xmonad =<< statusBar myBar myPP statusbarToggleKey myConfig
