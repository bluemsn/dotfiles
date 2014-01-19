-- Imports
import XMonad
import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.ManageDocks
--import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- The main function
main = xmonad =<< statusBar myBar myPP statusbarToggleKey myConfig

myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#fe8019" "" . wrap "[" "]"
                , ppTitle = xmobarColor "#fe8019" "" }
statusbarToggleKey XConfig { XMonad.modMask = modMask  } = (modMask, xK_b)

myConfig = defaultConfig
      { modMask = mod4Mask
      , terminal = myTerminal
      , workspaces = myWorkspaces
      --, normalBorderColor = colorNormalBorder
      --, focusedBorderColor = colorFocusedBorder
      , borderWidth = 2
      --, keys = myKeys
      }

myTerminal = "urxvt"
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
--myKeys = [ ((modMask, xK_c), kill) ]
