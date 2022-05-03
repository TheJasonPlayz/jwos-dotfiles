import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.SetWMName

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

import XMonad.Hooks.EwmhDesktops

myTerminal      = "LIBGL_ALWAYS_SOFTWARE=1 alacritty"

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     $ myConfig

myConfig = def
    { terminal   = myTerminal
    , modMask    = mod4Mask      -- Rebind Mod to the Super key
    , manageHook = myManageHook  -- Match on certain windows
    , startupHook = myStartupHook
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-=", unGrab *> spawn "scrot -s"        )
    , ("M-]"  , spawn "firefox"                   )
    , ("M-p"  , spawn "rofi -show combi -combi-modes 'run,drun' -modes combi"                      )
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myStartupHook = do
	setWMName "LG3D"
        spawn "killall trayer"
        spawn "pulseaudio"
        spawn "picom"
        spawn "nm-applet"
        spawn "volumeicon"
        spawn "emacs --daemon"
        spawn "cbatticon"
        spawn "redshift -l 38.973320:-104.622971"
	
        spawn " sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --height 22 --iconspacing 5"
        spawn "nitrogen --restore &"

