--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
--import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myManageHook = composeAll
	[ className =? "Gimp"      --> doFloat
	, className =? "Vncviewer" --> doFloat
	]

main = do
	xmproc <- spawnPipe "xmobar" -- Start xmobar
	xmonad $ defaultConfig
		{ manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
		, layoutHook = avoidStruts  $  layoutHook defaultConfig
		, logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc
			, ppTitle = xmobarColor "green" "" . shorten 50
			}
		, modMask = mod4Mask     -- Rebind Mod to the Windows key
		, terminal = "urxvt"     -- Set default terminal to urxvt
		}
