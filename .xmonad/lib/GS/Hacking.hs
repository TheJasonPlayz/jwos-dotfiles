module GS.Hacking where 
import XMonad
import XMonad.Actions.GridSelect
import Other.Vars

hackingKeybindings=
  [
    ("Webapp", "xdotool key super+alt+shift+0")
  , ("Fuzzer", "xdotool key super+alt+shift+1")
  , ("Scanner", "xdotool key super+alt+shift+2")
  , ("Proxy", "xdotool key super+alt+shift+3")
  , ("Windows", "xdotool key super+alt+shift+4")
  , ("Dos", "xdotool key super+alt+shift+5")
  , ("Disassembler", "xdotool key super+alt+shift+6")
  , ("Cracker", "xdotool key super+alt+shift+7")
  , ("Voip", "xdotool key super+alt+shift+8")
  , ("Exploitation", "xdotool key super+alt+shift+9")
  , ("Exploitation", "xdotool key super+alt+shift+9")
  , ("Spoof", "xdotool key super+alt+shift+1+1")
  , ("Forensic", "xdotool key super+alt+shift+1+2")
  , ("Crypto", "xdotool key super+alt+shift+1+3")
  , ("Backdoor", "xdotool key super+alt+shift+1+4")
  , ("Networking", "xdotool key super+alt+shift+1+5")
  , ("Misc", "xdotool key super+alt+shift+1+6")
  , ("Defensive", "xdotool key super+alt+shift+1+7")
  , ("Wireless", "xdotool key super+alt+shift+1+8")
  , ("Automation", "xdotool key super+alt+shift+1+9")
  , ("Sniffer", "xdotool key super+alt+shift+2+0")
  , ("Binary", "xdotool key super+alt+shift+2+1")
  , ("Packer", "xdotool key super+alt+shift+2+2")
  , ("Reversing", "xdotool key super+alt+shift+2+3")
  , ("Mobile", "xdotool key super+alt+shift+2+4")
  , ("Malware", "xdotool key super+alt+shift+2+5")
  , ("Code_audit", "xdotool key super+alt+shift+2+6")
  , ("Social", "xdotool key super+alt+shift+2+7")
  , ("Honeypot", "xdotool key super+alt+shift+2+8")
  , ("Hardware", "xdotool key super+alt+shift+2+9")
  , ("Fingerprint", "xdotool key super+alt+shift+3+0")
  , ("Decompiler", "xdotool key super+alt+shift+3+1")
  , ("Config", "xdotool key super+alt+shift+3+2")
  , ("Debugger", "xdotool key super+alt+shift+3+3")
  , ("Firmware", "xdotool key super+alt+shift+3+4")
  , ("Bluetooth", "xdotool key super+alt+shift+3+5")
  , ("Database", "xdotool key super+alt+shift+3+6")
  , ("Automobile", "xdotool key super+alt+shift+3+7")
  , ("Nfc", "xdotool key super+alt+shift+3+8")
  , ("Tunnel", "xdotool key super+alt+shift+3+9")
  , ("Drone", "xdotool key super+alt+shift+4+0")
  , ("Unpacker", "xdotool key super+alt+shift+4+1")
  , ("Radio", "xdotool key super+alt+shift+4+2")
  , ("Keylogger", "xdotool key super+alt+shift+4+3")
  , ("Stego", "xdotool key super+alt+shift+4+4")
  , ("Anti_forensic", "xdotool key super+alt+shift+4+5")
  , ("Ids", "xdotool key super+alt+shift+4+6")
  , ("Gpu", "xdotool key super+alt+shift+4+7")
 ]
 
gsWebapp=
  [
        ("Burp Suite", "burpsuite")
      , ("ZAProxy", "zaproxy")
   ]
gsFuzzer=
  [

  ]
gsScanner=
  [
        ("NMap", myTerminal ++ "--hold -e" ++ "nmap --help")
  ]
gsProxy=
  [

  ]
gsWindows=
  [

  ]
gsDos=
  [

  ]
gsDisassembler=
  [

  ]
gsCracker=
  [

  ]
gsVoip=
  [

  ]
gsExploitation=
  [

  ]
gsRecon=
  [

  ]
gsSpoof=
  [

  ]
gsForensic=
  [

  ]
gsCrypto=
  [

  ]
gsBackdoor=
  [

  ]
gsNetworking=
  [
    ("Netcat", myTerminal ++ "--hold -e" ++ "nc -h")
  ]
gsMisc=
  [

  ]
gsDefensive=
  [

  ]
gsWireless=
  [

  ]
gsAutomation=
  [

  ]
gsSniffer=
  [

  ]
gsBinary=
  [

  ]
gsPacker=
  [

  ]
gsReversing=
  [

  ]
gsMobile=
  [

  ]
gsMalware=
  [

  ]
gsCode_audit=
  [

  ]
gsSocial=
  [

  ]
gsHoneypot=
  [

  ]
gsHardware=
  [

  ]
gsFingerprint=
  [

  ]
gsDecompiler=
  [

  ]
gsConfig=
  [

  ]
gsDebugger=
  [

  ]
gsFirmware=
  [

  ]
gsBluetooth=
  [

  ]
gsDatabase=
  [
	("SQLMap", myTerminal ++ "--hold -e" ++ "sqlmap")
  ]
gsAutomobile=
  [

  ]
gsNfc=
  [

  ]
gsTunnel=
  [

  ]
gsDrone=
  [

  ]
gsUnpacker=
  [

  ]
gsRadio=
  [

  ]
gsKeylogger=
  [

  ]
gsStego=
  [

  ]
gsAnti_forensic=
  [

  ]
gsIds=
  [

  ]
gsGpu=
  [

  ]

gsAllHacking = gsWebapp ++ gsFuzzer ++ gsScanner ++ gsProxy ++ gsWindows ++ gsDos ++ gsDisassembler ++ gsCracker ++ gsVoip ++ gsExploitation ++ gsRecon ++ gsSpoof ++ gsForensic ++ gsCrypto ++ gsBackdoor ++ gsNetworking ++ gsMisc ++ gsDefensive ++ gsWireless ++ gsAutomation ++ gsSniffer ++ gsBinary ++ gsPacker ++ gsReversing ++ gsMobile ++ gsMalware ++ gsCode_audit ++ gsSocial ++ gsHoneypot ++ gsHardware ++ gsFingerprint ++ gsDecompiler ++ gsConfig ++ gsDebugger ++ gsFirmware ++ gsBluetooth ++ gsDatabase ++ gsAutomobile ++ gsNfc ++ gsTunnel ++ gsDrone ++ gsUnpacker ++ gsRadio ++ gsKeylogger ++ gsStego ++ gsAnti_forensic ++ gsIds ++ gsGpu
