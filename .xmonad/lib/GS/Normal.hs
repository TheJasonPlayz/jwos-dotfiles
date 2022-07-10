module GS.Normal where 
import GS.Functions
import XMonad
import XMonad.Actions.GridSelect
import Other.Vars
                 
gsNormalCategories =
  [ 
    ("Favorites",  "xdotool key super+alt+f")
  , ("Internet",   "xdotool key super+alt+1")
  , ("Multimedia", "xdotool key super+alt+2")
  , ("Office",     "xdotool key super+alt+3")
  , ("Settings",   "xdotool key super+alt+4")
  , ("System",     "xdotool key super+alt+5")
  , ("Utilities",  "xdotool key super+alt+6")
  , ("Hacking",    "xdotool key super+alt+7")
  ]
gsFavorites =
  [
      ("Alacritty", myTerminal)
    , ("PCManFM", "pcmanfm")
    , ("Emacsclient", "emacsclient -c -a 'emacs'")
    , ("Firefox", "firefox")
    , ("Discord", "discord")
    , ("LXAppearance", "lxappearance") 
    , ("OneDrive", "firefox https://onedrive.live.com")
    , ("Evince", "evince")
    , ("VSCodium", "codium")
    , ("Obsidian", "obsidian")
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
