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

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "LIBGL_ALWAYS_SOFTWARE=1 alacritty"

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     $ myConfig

myConfig = def
    { terminal   = myTerminal
    , focusedBorderColor = "#add8e6"
    , modMask    = mod4Mask
    , manageHook = myManageHook
    , startupHook = myStartupHook
    , mouseBindings = myMouseBindings
    , keys = myKeys
    }

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myStartupHook = do
        setWMName "LG3D"
        spawn "killall trayer"
        spawn "kill volumeicon"
        spawn "pulseaudio"
        spawn "picom"
        spawn "nm-applet"       
        spawn "volumeicon"
        spawn "emacs --daemon"
        spawn "cbatticon"
        spawn "redshift -l 38.973320:-104.622971"
        spawn "sudo mount -t vboxsf Shared_Folder /mnt/sf/"

        spawn " sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --height 22 --iconspacing 5"
        spawn "nitrogen --restore &"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn myTerminal) -- spawn terminal
    , ((modMask, xK_t), withFocused $ windows . W.sink) -- sink window into tiling
    , ((modMask, xK_n), spawn "pcmanfm") -- run file manager
    , ((modMask, xK_p), spawn "rofi -show combi -combi-modes 'window,drun,run,ssh' -modes combi") -- run rofi
    , ((modMask .|. shiftMask, xK_c), kill) -- kill focused window
    , ((modMask .|. shiftMask, xK_q), spawn "killall trayer volumeicon nm-applet && xmonad --recompile && xmonad --restart") -- restart xmonad
    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]
    
