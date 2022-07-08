module GS.Hacking where 
import XMonad
import XMonad.Actions.GridSelect
import Other.Vars
  
gsHackingCategories = 
  [
    ("Info Gathering", "xdotool key super+shift+alt+1")
  , ("Vunerability Analysis", "xdotool key super+shift+alt+2")
  , ("Web Application Analysis", "xdotool key super+shift+alt+3")
  , ("Database Assessment", "xdotool key super+shift+alt+4")
  , ("Password Attacks", "xdotool key super+shift+alt+5")
  , ("Wireless Attacks", "xdotool key super+shift+alt+6")
  , ("Reverse Engineering", "xdotool key super+shift+alt+7")
  , ("Exploitation Tools", "xdotool key super+shift+alt+8")
  , ("Sniffing & Spoofing", "xdotool key super+shift+alt+9")
  , ("Post Exploitation", "xdotool key super+shift+alt+1+0")
  , ("Forensics", "xdotool key super+shift+alt+1+1")
  , ("Reporting Tools", "xdotool key super+shift+alt+1+2")
  , ("Social Engineering Tools", "xdotool key super+shift+alt+3")
  ]
gsIG = 
  [ 
    ("W.I.P", myTerminal ++ " --hold -e " ++ "nmap")
  
  ]
gsVA = 
  [ 
  ]
gsWAA = 
  [ 
  ]
gsDA = 
  [ 
  ]
gsPA = 
  [ 
  ]
gsWA = 
  [ 
  ]
gsRE = 
  [ 
  ]
gsET = 
  [ 
  ]
gsSandS = 
  [ 
  ]
gsPE = 
  [ 
  ]
gsF = 
  [ 
  ]
gsRT = 
  [ 
  ]
gsSE = 
  [ 
  ]
gsAllHacking = gsIG ++ gsVA ++ gsWAA ++ gsDA ++ gsPA ++ gsWA ++ gsRE ++ gsET ++ gsSandS ++ gsPE ++ gsF ++ gsRT ++ gsSE
