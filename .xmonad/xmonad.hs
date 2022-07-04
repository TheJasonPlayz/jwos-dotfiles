import Colors.DoomOne
import qualified Data.Map as M
import Data.Maybe
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

myTerminal = "LIBGL_ALWAYS_SOFTWARE=1 alacritty"

myEmacs = "emacsclient -c -a 'emacs'"

myBorderWidth = 2

myNormColor = "#afafaf"

myFocusColor = "#fafafa"

myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                (0x28,0x2c,0x34) -- lowest inactive bg
                (0x28,0x2c,0x34) -- highest inactive bg
                (0xc7,0x92,0xea) -- active bg
                (0xc0,0xa7,0x9a) -- inactive fg
                (0x28,0x2c,0x34) -- active fg

main :: IO ()
main = do
  xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.config/xmobar/.xmobarrc")
  xmonad $
    addDescrKeys ((mod4Mask .|. mod1Mask, xK_h), showKeybindings) myKeys $
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

normWorkspaces = ["chat", "www", "dev", "music", "img", "vid", "gaming", "writing", "configs"]

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

  spawn " sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --height 19 --iconspacing 5"
  spawn "nitrogen --restore &"

myLayoutHook = avoidStruts (hiddenWindows (tiled ||| Mirror tiled ||| Full ||| threeCol ||| Mirror threeCol ||| spirals ||| Mirror spirals ||| Grid))
  where
    threeCol = ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    spirals = spiral (6 / 7)
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $
  io $ do
    h <- spawnPipe $ "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
    hPutStr h (unlines $ showKm x)
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
  (subtitle "Custom Keys" :) $ mkNamedKeymap c $
      [ -- XMonad
        ("M-S-k", addName "" $ spawn "killall trayer volumeicon nm-applet"),
        ("M-S-r", addName "" $ spawn "xmonad --recompile && xmonad --restart"),
        -- Programs
        ("M-S-<Return>", addName "" $ spawn myTerminal),
        ("M-f", addName "" $ spawn "pcmanfm"),
        ("M-e", addName "" $ spawn myEmacs),
        ("M-p", addName "" $ spawn "rofi -show combi"),
        ("M-a", addName "" $ spawn "alsamixer"),
        -- Workspaces
        ("M-<Right>", addName "" $ nextWS),
        ("M-<Left>", addName "" $ prevWS),
        ("M-<KP_Add>", addName "" $ shiftToNext),
        ("M-<KP_Subtract>", addName "" $ shiftToPrev),
        ("M-S-<KP_Add>", addName "" $ shiftToNext >> nextWS),
        ("M-S-<KP_Subtract>", addName "" $ shiftToPrev >> prevWS),
        -- Windows
        ("M-S-c", addName "" $ kill),
        ("M-h", addName "" $ withFocused hideWindow),
        ("M-S-h", addName "" $ popOldestHiddenWindow),
        ("M-<Return>", addName "" $ windows W.focusMaster)

        -- Increase/decrease spacing
        , ("C-M1-j", addName "Decrease window spacing" $ decWindowSpacing 4)
        , ("C-M1-k", addName "Increase window spacing" $ incWindowSpacing 4)
        , ("C-M1-h", addName "Decrease screen spacing" $ decScreenSpacing 4)
        , ("C-M1-l", addName "Increase screen spacing" $ incScreenSpacing 4)


        -- Window resizing
        , ("M-s", addName "Shrink window"               $ sendMessage Shrink)
        , ("M-x", addName "Expand window"               $ sendMessage Expand)
        , ("M-M1-s", addName "Shrink window vertically" $ sendMessage MirrorShrink)
        , ("M-M1-x", addName "Expand window vertically" $ sendMessage MirrorExpand)


        -- Layouts
        , ("M-<Space>", addName "" $ sendMessage NextLayout)
        , ("M-.", addName "" $ sendMessage (IncMasterN 1))
        , ("M-,", addName "" $ sendMessage (IncMasterN (-1)))

        , ("M-m", addName "Move focus to master window" $ windows W.focusMaster)
        , ("M-j", addName "Move focus to next window"   $ windows W.focusDown)
        , ("M-k", addName "Move focus to prev window"   $ windows W.focusUp)
        , ("M-S-m", addName "Swap focused window with master window" $ windows W.swapMaster)
        , ("M-S-j", addName "Swap focused window with next window"   $ windows W.swapDown)
        , ("M-S-k", addName "Swap focused window with prev window"   $ windows W.swapUp)
        , ("M-<Backspace>", addName "Move focused window to master"  $ promote)
        , ("M-S-<Tab>", addName "Rotate all windows except master"   $ rotSlavesDown)
        , ("M-C-<Tab>", addName "Rotate all windows current stack"   $ rotAllDown)

        -- Grid Select
        , ("M-M1-<Return>", addName "Select favorite apps" $ spawnSelected'
       $   gsInternet ++ gsMultimedia ++ gsOffice ++ gsSettings ++ gsSystem ++ gsUtilities)
        , ("M-M1-c", addName "Select favorite apps" $ spawnSelected' gsCategories)
        , ("M-M1-t", addName "Goto selected window"        $ goToSelected $ mygridConfig myColorizer)
        , ("M-M1-b", addName "Bring selected window"       $ bringSelected $ mygridConfig myColorizer)
        , ("M-M1-3", addName "Menu of Internet apps"       $ spawnSelected' gsInternet)
        , ("M-M1-4", addName "Menu of multimedia apps"     $ spawnSelected' gsMultimedia)
        , ("M-M1-5", addName "Menu of office apps"         $ spawnSelected' gsOffice)
        , ("M-M1-6", addName "Menu of settings apps"       $ spawnSelected' gsSettings)
        , ("M-M1-7", addName "Menu of system apps"         $ spawnSelected' gsSystem)
        , ("M-M1-8", addName "Menu of utilities apps"      $ spawnSelected' gsUtilities)

      ]

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_slash) , substringSearch myNavigation)
         ,((0,xK_Left)  , move (-1,0)  >> myNavigation)
         ,((0,xK_h)     , move (-1,0)  >> myNavigation)
         ,((0,xK_Right) , move (1,0)   >> myNavigation)
         ,((0,xK_l)     , move (1,0)   >> myNavigation)
         ,((0,xK_Down)  , move (0,1)   >> myNavigation)
         ,((0,xK_j)     , move (0,1)   >> myNavigation)
         ,((0,xK_Up)    , move (0,-1)  >> myNavigation)
         ,((0,xK_k)     , move (0,-1)  >> myNavigation)
         ,((0,xK_y)     , move (-1,-1) >> myNavigation)
         ,((0,xK_i)     , move (1,-1)  >> myNavigation)
         ,((0,xK_n)     , move (-1,1)  >> myNavigation)
         ,((0,xK_m)     , move (1,-1)  >> myNavigation)
         ,((0,xK_space) , setPos (0,0) >> myNavigation)
         ]
       navDefaultHandler = const myNavigation

mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_navigate    = myNavigation
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 180
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

runSelectedAction' :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction' conf actions = do
    selectedActionM <- gridselect conf actions
    case selectedActionM of
        Just selectedAction -> selectedAction
        Nothing -> return ()

gsCategories =
  [ ("Internet",   "xdotool key super+alt+1")
  , ("Multimedia", "xdotool key super+alt+2")
  , ("Office",     "xdotool key super+alt+3")
  , ("Settings",   "xdotool key super+alt+4")
  , ("System",     "xdotool key super+alt+5")
  , ("Utilities",  "xdotool key super+alt+6")
  ]

gsInternet =
  [ ("Firefox", "firefox")
  , ("Discord", "discord")
  , ("Element", "element-desktop")
  , ("LBRY App", "lbry")
  , ("Mailspring", "mailspring")
  , ("Nextcloud", "nextcloud")
  , ("Transmission", "transmission-gtk")
  , ("Zoom", "zoom")
  ]

gsMultimedia =
  [ ("Audacity", "audacity")
  , ("Blender", "blender")
  , ("Deadbeef", "deadbeef")
  , ("Kdenlive", "kdenlive")
  , ("OBS Studio", "obs")
  , ("VLC", "vlc")
  ]

gsOffice =
  [ ("Document Viewer", "evince")
  , ("LibreOffice", "libreoffice")
  , ("LO Base", "lobase")
  , ("LO Calc", "localc")
  , ("LO Draw", "lodraw")
  , ("LO Impress", "loimpress")
  , ("LO Math", "lomath")
  , ("LO Writer", "lowriter")
  ]

gsSettings =
  [ ("Customize Look and Feel", "lxappearance")
  ]

gsSystem =
  [ ("Alacritty", myTerminal)
  , ("Bash", (myTerminal ++ " -e bash"))
  , ("Htop", (myTerminal ++ " -e htop"))
  , ("Fish", (myTerminal ++ " -e fish"))
  , ("PCManFM", "pcmanfm")
  , ("VirtualBox", "virtualbox")
  , ("Virt-Manager", "virt-manager")
  , ("Zsh", (myTerminal ++ " -e zsh"))
  ]

gsUtilities =
  [ ("Emacs", "emacs")
  , ("Emacsclient", "emacsclient -c -a 'emacs'")
  , ("Nitrogen", "nitrogen")
  ]
