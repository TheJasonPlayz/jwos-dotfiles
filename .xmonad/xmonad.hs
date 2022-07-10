-- Possible choices are:
      -- DoomOne
      -- Dracula
      -- GruvboxDark
      -- MonokaiPro
      -- Nord
      -- OceanicNext
      -- Palenight
      -- SolarizedDark
      -- SolarizedLight
      -- TomorrowNight

import GS.Normal
import GS.Hacking
import GS.Functions
import Other.Vars
import Colors.DoomOne
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Grid
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedActions
import XMonad.Util.Run
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

main :: IO ()
main = do
  xmproc0 <- spawnPipe ("xmobar -x 0 ~/.config/xmobar/doom-one-xmobarrc")
  xmonad $
    addDescrKeys ((mod1Mask, xK_h), showKeybindings) myKeys $
      ewmh
        def
          { manageHook = myManageHook <+> manageDocks,
            modMask = mod4Mask,
            terminal = myTerminal,
            startupHook = myStartupHook,
            layoutHook = myLayoutHook,
            workspaces = myWorkspaces,
            borderWidth = myBorderWidth,
            normalBorderColor = myNormColor,
            focusedBorderColor = myFocusColor,
            logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = \x -> hPutStrLn xmproc0 x, -- xmobar on monitor 1
              ppCurrent =
              xmobarColor color06 "" . wrap ("<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">")
                          "</box>",
                    -- Visible but not current workspace
                    ppVisible = xmobarColor color06 "" . clickable,
                    -- Hidden workspace
                    ppHidden =
                      xmobarColor color05 ""
                        . wrap
                          ("<box type=Top width=2 mt=2 color=" ++ color05 ++ ">")
                          "</box>"
                        . clickable,
                    -- Hidden workspaces (no windows)
                    ppHiddenNoWindows = xmobarColor color05 "" . clickable,
                    -- Title of active window
                    ppTitle = xmobarColor color16 "" . shorten 60,
                    -- Separator character
                    ppSep = "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>",
                    -- Urgent workspace
                    ppUrgent = xmobarColor color02 "" . wrap "!" "!",
                    -- Adding # of windows on current workspace to the bar
                    ppExtras = [windowCount],
                    -- order of things in xmobar
                    ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                  }
          }

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

normWorkspaces = ["chat", "dev", "www", "hacking", "music", "img", "vid", "gaming", "writing", "configs"]

myWorkspaces = normWorkspaces

hackathonWorkspaces = ["www", "chat", "zoom", "design", "dev", "present", "vid", "img", "music"]

myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1 ..]

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      isDialog --> doFloat
    ]

myStartupHook = do
  -- Set Window Manager Name
  setWMName "LG3D"
  
  -- Background / System Tray
  spawn "pulseaudio"
  spawn "picom"
  spawn "nm-applet"
  spawn "volumeicon"
  spawn "cbatticon"
  spawn "redshift -l 38.973320:-104.622971"
  
  -- Apps
  spawn "flameshot"
  spawn "discord"
  spawn "emacs --daemon"
  
  -- Conky and Trayer
  spawnOnce ("sleep 2 && conky -c $HOME/.config/conky/xmonad/" ++ colorScheme ++ "-91.conkyrc")
  spawnOnce ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --iconspacing 5 --alpha 0 --height 22 " ++ colorTrayer)

  spawn "nitrogen --restore &"

myLayoutHook = avoidStruts (hiddenWindows (tiled ||| Mirror tiled ||| Full ||| threeCol ||| Mirror threeCol ||| spirals ||| Mirror spirals ||| Grid))
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    spirals = spiral (6 / 7)
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

subtitle' ::  String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper
                      $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where
    sep = replicate (6 + length x) '-'

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#46d9ff --back=#000000 --center --geometry=1200x800 --title \"XMonad keybindings\""
  --hPutStr h (unlines $ showKm x) -- showKM adds ">>" before subtitles
  hPutStr h (unlines $ showKmSimple x) -- showKmSimple doesn't add ">>" to subtitles
  hClose h
  return ()

myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        ( \w ->
            focus w >> mouseMoveWindow w
              >> windows W.shiftMaster
        )
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        ( \w ->
            focus w >> mouseResizeWindow w
              >> windows W.shiftMaster
        )
      )
    ]

myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
      let subKeys str ks = subtitle' str : mkNamedKeymap c ks in
      
      subKeys "XMonad Essentials"
      [ ("M-S-k", addName "" $ spawn "killall trayer volumeicon nm-applet"),
        ("M-C-r", addName "" $ spawn "xmonad --recompile"), 
        ("M-S-r", addName "" $ spawn "xmonad --restart"),
        ("M-S-c", addName "" $ kill) ]
        
      ^++^ subKeys "Programs"
        [ ("M-S-<Return>", addName "" $ spawn myTerminal),
        ("M-f", addName "" $ spawn "pcmanfm"),
        ("M-e", addName "" $ spawn myEmacs),
        ("M-p", addName "" $ spawn "rofi -show combi"),
        ("M-a", addName "" $ spawn "alsamixer"),
        ("M-c", addName "" $ spawn "conky -c ~/.config/conky/xmonad/doom-one-01.conkyrc"),
        ("M-C-c", addName "" $ spawn "killall conky") ]

        ^++^ subKeys "Workspaces"
        [ ("M-<Right>", addName "" $ nextWS),
        ("M-<Left>", addName "" $ prevWS),
        ("M-<KP_Add>", addName "" $ shiftToNext),
        ("M-<KP_Subtract>", addName "" $ shiftToPrev),
        ("M-S-<KP_Add>", addName "" $ shiftToNext >> nextWS),
        ("M-S-<KP_Subtract>", addName "" $ shiftToPrev >> prevWS) ]
        
        ^++^ subKeys "Windows"
        [ ("M-h", addName "" $ withFocused hideWindow),
        ("M-S-h", addName "" $ popOldestHiddenWindow),
        ("M-<Return>", addName "" $ windows W.focusMaster) ]

        ^++^ subKeys "Spacing"
        [ ("C-M1-j", addName "Decrease window spacing" $ decWindowSpacing 4),
        ("C-M1-k", addName "Increase window spacing" $ incWindowSpacing 4),
        ("C-M1-h", addName "Decrease screen spacing" $ decScreenSpacing 4),
        ("C-M1-l", addName "Increase screen spacing" $ incScreenSpacing 4) ]


        ^++^ subKeys "Window Sizing"
        [ ("M-s", addName "Shrink window"               $ sendMessage Shrink),
        ("M-x", addName "Expand window"               $ sendMessage Expand),
        ("M-M1-s", addName "Shrink window vertically" $ sendMessage MirrorShrink),
        ("M-M1-x", addName "Expand window vertically" $ sendMessage MirrorExpand) ]


        ^++^ subKeys "Switch Layouts & Master Commands"
        [ ("M-<Space>", addName "" $ sendMessage NextLayout),
        ("M-.", addName "" $ sendMessage (IncMasterN 1)),
        ("M-,", addName "" $ sendMessage (IncMasterN (-1))) ]

        ^++^ subKeys "Layout Commands"
        [ ("M-m", addName "Move focus to master window" $ windows W.focusMaster),
        ("M-j", addName "Move focus to next window"   $ windows W.focusDown),
        ("M-k", addName "Move focus to prev window"   $ windows W.focusUp),
        ("M-S-m", addName "Swap focused window with master window" $ windows W.swapMaster),
        ("M-S-j", addName "Swap focused window with next window"   $ windows W.swapDown),
        ("M-S-k", addName "Swap focused window with prev window"   $ windows W.swapUp),
        ("M-<Backspace>", addName "Move focused window to master"  $ promote),
        ("M-S-<Tab>", addName "Rotate all windows except master"   $ rotSlavesDown),
        ("M-C-<Tab>", addName "Rotate all windows current stack"   $ rotAllDown) ]
        
        ^++^ subKeys "Normal Grid Select"
        [ ("M-M1-<Return>", addName "Select all" $ spawnSelected'
        $   gsInternet ++ gsMultimedia ++ gsOffice ++ gsSettings ++ gsSystem ++ gsUtilities ++ gsAllHacking),
        ("M-M1-f", addName "Select favorite apps" $ spawnSelected' gsFavorites),
        ("M-M1-c", addName "Select all categories" $ spawnSelected' gsNormalCategories),
        ("M-M1-t", addName "Goto selected window"        $ goToSelected $ mygridConfig myColorizer),
        ("M-M1-b", addName "Bring selected window"       $ bringSelected $ mygridConfig myColorizer),
        ("M-M1-1", addName "Menu of Internet apps"       $ spawnSelected' gsInternet),
        ("M-M1-2", addName "Menu of multimedia apps"     $ spawnSelected' gsMultimedia),
        ("M-M1-3", addName "Menu of office apps"         $ spawnSelected' gsOffice),
        ("M-M1-4", addName "Menu of settings apps"       $ spawnSelected' gsSettings),
        ("M-M1-5", addName "Menu of system apps"         $ spawnSelected' gsSystem),
        ("M-M1-6", addName "Menu of utilities apps"      $ spawnSelected' gsUtilities),
        ("M-M1-7", addName "Menu of hacking apps"        $ spawnSelected' gsHackingCategories) ]
        
        ^++^ subKeys "Hacking Grid Select"
        [ ("M-M1-S-1", addName "Info Gathering" $ spawnSelected' gsIG),
        ("M-M1-S-2", addName "Vunerability Analysis" $ spawnSelected' gsVA),
        ("M-M1-S-3", addName "Web Application Analysis" $ spawnSelected' gsWAA),
        ("M-M1-S-4", addName "Database Assessment" $ spawnSelected' gsDA),
        ("M-M1-S-5", addName "Password Attacks" $ spawnSelected' gsPA),
        ("M-M1-S-6", addName "Wireless Attacks" $ spawnSelected' gsWA),
        ("M-M1=S-7", addName "Reverse Engineering" $ spawnSelected' gsRE),
        ("M-S-M1-8", addName "Exploitation Tools" $ spawnSelected' gsET),
        ("M-S-M1-9", addName "Sniffing & Spoofing" $ spawnSelected' gsSandS),
        ("M-S-M1-1-0", addName "Post Exploitation" $ spawnSelected' gsPE),
        ("M-S-M1-1-1", addName "Forensics" $ spawnSelected' gsF),
        ("M-S-M1-1-2", addName "Reporting Tools" $ spawnSelected' gsRT),
        ("M-S-M1-1-3", addName "Social Engineering Tools" $ spawnSelected' gsSE) ]
