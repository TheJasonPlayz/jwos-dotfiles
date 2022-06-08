import XMonad

import XMonad.Actions.CycleWS

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.SetWMName

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Hidden

import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "LIBGL_ALWAYS_SOFTWARE=1 alacritty"
myWorkspaces    = ["1:irc", "2", "3", "4", "5", "6", "7", "8", "9"]

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { terminal   = myTerminal
    , focusedBorderColor = "#add8e6"
    , modMask    = mod4Mask
    , manageHook = myManageHook
    , startupHook = myStartupHook
    , mouseBindings = myMouseBindings
    , keys = myKeys
    , layoutHook = myLayout
    , workspaces = myWorkspaces
    }

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""


myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myStartupHook = do
        setWMName "LG3D"
        spawn "killall trayer"
        spawn "kill volumeicon nm-applet"
        spawn "pulseaudio"
        spawn "picom"
        spawn "nm-applet"       
        spawn "volumeicon"
        spawn "emacs --daemon"
        spawn "cbatticon"
        spawn "redshift -l 38.973320:-104.622971"
        spawn "sudo mount -t vboxsf Shared_Folder /mnt/sf/"

        spawn " sleep 2 && trayer --edge top --align right --width 10 --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --height 19 --iconspacing 5"
        spawn "nitrogen --restore &"

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn myTerminal) -- spawn terminal
    , ((modMask, xK_t), withFocused $ windows . W.sink) -- sink window into tiling
    , ((modMask, xK_n), spawn "pcmanfm") -- run file manager
    , ((modMask, xK_p), spawn "rofi -show combi -combi-modes 'window,drun,run,ssh' -modes combi") -- run rofi
    , ((modMask .|. shiftMask, xK_c), kill) -- kill focused window
    , ((modMask .|. shiftMask, xK_q), spawn "killall trayer volumeicon nm-applet && xmonad --recompile && xmonad --restart") -- restart xmonad
    , ((modMask, xK_space), sendMessage NextLayout) -- Switch to next layout
    , ((modMask, xK_Left), prevWS) -- switch to previous workspace
    , ((modMask, xK_Right), nextWS) -- switch to next workspace
    , ((modMask, xK_h), withFocused hideWindow) -- Hide focused window
    , ((modMask .|. shiftMask, xK_h), popOldestHiddenWindow) -- Pop out oldest hidden window
    , ((modMask, xK_comma), sendMessage (IncMasterN 1)) -- increment the number of windows in the master area
    , ((modMask, xK_period), sendMessage (IncMasterN (-1))) -- deincrement the number of windows in the master area
    , ((modMask, xK_Return), windows W.focusMaster) -- move focus to the master window
    --, ((modMask .|. controlMask, xK_Home), spawn "mpc toggle") -- play/pause song
    --, ((modMask .|. controlMask, xK_End), spawn "mpc stop") -- stop playback
    --, ((modMask .|. controlMask, xK_Prior), spawn "mpc prev") -- previous songch
    --, ((modMask .|. controlMask, xK_Next), spawn "mpc next") -- next song		
    , ((modMask, xK_e), spawn "emacsclient -c -a 'emacs'")
    
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

myLayout = hiddenWindows (tiled ||| Mirror tiled ||| Full ||| threeCol ||| Mirror threeCol ||| spirals ||| Mirror spirals ||| Grid)
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    spirals = spiral (6/7)
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
