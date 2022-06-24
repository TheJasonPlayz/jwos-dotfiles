import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.Run
import XMonad.Util.NamedActions

import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutModifier

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe
import System.IO

import Colors.DoomOne

myTerminal      = "LIBGL_ALWAYS_SOFTWARE=1 alacritty"
myEmacs         = "emacsclient -c -a 'emacs'"
myWorkspaces    = ["chat", "dev", "www", "gaming", "writing", "6", "7", "8", "9"]
hackathonWorkspaces = ["brainstorming", "prototyping", "design", "dev"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]
myBorderWidth   = 2
myNormColor     = "#afafaf"
myFocusColor    = "#fafafa"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


main :: IO ()
main = do
    xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.xmobarrc")
    xmonad $ addDescrKeys ((mod4Mask, xK_F2), showKeybindings) myKeys $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
       , modMask            = mod4Mask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $ xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc0 x   -- xmobar on monitor 1
        , ppCurrent = xmobarColor color06 "" . wrap
                      ("<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">") "</box>"
          -- Visible but not current workspace
        , ppVisible = xmobarColor color06 "" . clickable
          -- Hidden workspace
        , ppHidden = xmobarColor color05 "" . wrap
                     ("<box type=Top width=2 mt=2 color=" ++ color05 ++ ">") "</box>" . clickable
          -- Hidden workspaces (no windows)
        , ppHiddenNoWindows = xmobarColor color05 ""  . clickable
          -- Title of active window
        , ppTitle = xmobarColor color16 "" . shorten 60
          -- Separator character
        , ppSep =  "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
          -- Urgent workspace
        , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
          -- Adding # of windows on current workspace to the bar
        , ppExtras  = [windowCount]
          -- order of things in xmobar
        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        }
        }

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()


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

myLayoutHook = avoidStruts (hiddenWindows (tiled ||| Mirror tiled ||| Full ||| threeCol ||| Mirror threeCol ||| spirals ||| Mirror spirals ||| Grid))
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    spirals = spiral (6/7)
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

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

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
      [
      -- XMonad
        ("M-S-k", addName "" $ spawn "killall trayer volumeicon nm-applet")
      , ("M-S-r", addName "" $ spawn "xmonad --recompile && xmonad --restart")
      -- Programs
      , ("M-S-<Return>", addName "" $ spawn myTerminal)
      , ("M-f", addName "" $ spawn "pcmanfm")
      , ("M-e", addName "" $ spawn myEmacs)
      , ("M-p", addName "" $ spawn "rofi -show combi")
      , ("M-a", addName "" $ spawn "alsamixer")
      -- Workspaces
      , ("M-<Right>", addName "" $ nextWS)
      , ("M-<Left>", addName "" $ prevWS)
      , ("M-<KP_Add>", addName "" $ shiftToNext)
      , ("M-<KP_Subtract>", addName "" $ shiftToPrev)

      -- Windows
      , ("M-S-c", addName "" $ kill)
      , ("M-h", addName "" $ withFocused hideWindow)
      , ("M-S-h", addName "" $ popOldestHiddenWindow)
      , ("M-<Return>", addName "" $ windows W.focusMaster)
      -- Layouts
      , ("M-<Space>", addName "" $ sendMessage NextLayout)
      , ("M-.", addName "" $ sendMessage (IncMasterN 1))
      , ("M-,", addName "" $ sendMessage (IncMasterN (-1)))
      ]
