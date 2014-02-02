{-# LANGUAGE FlexibleContexts #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows
import XMonad.Util.Run

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- MAIN

main :: IO ()
main = do
    d <- spawnPipe "dzen2 -x 0 -y 0 -w 980 -h 16 -fg '#fe8019' -bg '#282828' -fn '-*-dejavu sans-*-r-*-*-12-*-*-*-*-*-*-*' -ta l -u -e 'onstart=lower'"
    spawn $ "conky -c ~/.xmonad/conky_dzen | dzen2 -x 980 -y 0 -w 200 -h 16 -fg '#fdf4c1' -bg '#282828' -fn '-*-dejavu sans-*-r-*-*-12-*-*-*-*-*-*-*' -ta r -u -e 'onstart=lower' -p"
    xmonad $ withMyUrgencyHook $ defaultConfig {
        keys = myKeys <+> keys defaultConfig,
        layoutHook = myLayoutHook,
        logHook = myLogHook d,
        manageHook = myManageHook,
        modMask = mod4Mask,
        terminal = "urxvt"
    }

-- KEYS

myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [
     ((modm, xK_c), kill),
     ((modm, xK_p), spawn "dmenu-launch"),
     ((modm .|. shiftMask, xK_p), spawn "interrobang"),
     ((modm, xK_q), spawn "killall conky && killall dzen2 && if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    ]

-- STATUSBAR

myLogHook h = dynamicLogWithPP $ defaultPP {
    ppCurrent = dzenColor "#fdf4c1" "",
    ppHidden = dzenColor "#a89984" "",
    ppHiddenNoWindows = dzenColor "#504945" "",
    ppUrgent = dzenColor "#fb4934" "",
    ppSep = " :: ",
    ppWsSep = " ",
    ppTitle = dzenColor "#fdf4c1" "" . shorten 100,
    ppLayout = dzenColor "#a89984" "",
    ppOutput = hPutStrLn h
}

myManageHook = manageDocks <+> manageHook defaultConfig
myLayoutHook = avoidStruts $ layoutHook defaultConfig

-- URGENCY

withMyUrgencyHook :: LayoutClass l Window => XConfig l -> XConfig l
withMyUrgencyHook = withUrgencyHookC LibNotifyUrgencyHook
                  $ urgencyConfig { suppressWhen = Focused }

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]
