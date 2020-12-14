module MyKeys
(myKeys,myCheatsheetKey)
where

import System.Exit (exitSuccess)
import XMonad
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Util.EZConfig     -- Simpler keybinding syntax
import XMonad.Util.NamedActions -- Allows labelling of keybindings
import qualified XMonad.StackSet as W

-- Which programs to use as defaults
import MyPrograms

-- Convert multiword strings to arguments (concatenate with delimiters)
-- This makes sure my shell scripts correctly interpret their arguments
args :: String -> [String] -> String
args command arguments = command ++ " " ++ unwords (map show arguments)

-- M = M1 is Super, which I have also set to space when held down
-- M3 is Hyper, which I have set to Caps Lock
-- C-Esc is Super tapped on its own

myKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myKeys conf = let

    subKeys name list = subtitle name : mkNamedKeymap conf list

    -- Abbreviations for certain actions
    menuEditScript         = spawn $ args "menu-edit-script" [myMenu,myEditor]
    menuEditConfig         = spawn $ args "menu-edit-config" [myMenu,myEditor]
    menuChangeColourscheme = spawn $ args "menu-change-colourscheme" [myMenu]
    menuReadPdf            = spawn $ args "menu-read-pdf" [myMenu,myPdfReader]

    viewScreen s          = screenWorkspace s >>= flip whenJust (windows . W.view)
    shiftScreen s         = screenWorkspace s >>= flip whenJust (windows . W.shift)
    unFloat               = withFocused $ windows . W.sink

    volumeAdjust "toggle" = spawn "adjust-volume toggle"
    volumeAdjust value    = spawn $ args "adjust-volume" $ words value

    brightnessAdjust perc = spawn
        $ "xbacklight " ++ perc ++ " && notify-send \"Brightness `xbacklight -get`%\""

    in

    subKeys "Core"
    [ ("M-S-q",                   addName "Quit XMonad (logout)"   $ io exitSuccess)
    , ("M-q",                     addName "Recompile & restart"    $ spawn myBuildScript)
    , ("C-<Escape>",              addName "Application launcher"   $ spawn "appmenu")
    , ("M-S-c",                   addName "Close window"           $ kill)
    ] ^++^

    subKeys "Screens" (
    [("M-"++key,                  addName ("Focus screen "++show sc)   $ viewScreen sc)
        | (key,sc) <- zip ["w","e","r"] [0..]
    ] ^++^
    [("M-S-"++key,                addName ("Send to screen "++show sc) $ shiftScreen sc)
        | (key,sc) <- zip ["w","e","r"] [0..]
    ]) ^++^

    subKeys "Workspaces" (
    --[ ("M-u",                     addName "View next"              $ )
    --, ("M-i,",                    addName "View previous"          $ )
    --, ("M-S-u",                   addName "Send to next"           $ )
    --, ("M-S-i",                   addName "Send to previous"       $ )
    --] ^++^
    [ ("M-"++show key,            addName ("View workspace "++i)    $ windows $ W.greedyView i)
        | (key,i) <- zip [1..9] (XMonad.workspaces conf)
    ] ^++^
    [ ("M-S-"++show key,          addName ("Send to workspace "++i) $ windows $ W.shift i)
        | (key,i) <- zip [1..9] (XMonad.workspaces conf)
    ]) ^++^

    subKeys "Layouts"
    [ ("M-h",                     addName "Shrink master"          $ sendMessage Shrink)
    , ("M-l",                     addName "Expand master"          $ sendMessage Expand)
    , ("M-,",                     addName "Inc master windows"     $ sendMessage $ IncMasterN 1)
    , ("M-.",                     addName "Dec master windows"     $ sendMessage $ IncMasterN (-1))
    , ("M3-<Space>",              addName "Next layout"            $ sendMessage NextLayout)
    , ("M-f",                     addName "Toggle fullscreen"      $ sendMessage $ Toggle NBFULL)
    ] ^++^

    subKeys "Windows"
    [ ("M-<Tab>",                 addName "Focus next"             $ windows W.focusDown)
    , ("M-S-<Tab>",               addName "Focus previous"         $ windows W.focusUp)
    , ("M-j",                     addName "Focus next"             $ windows W.focusDown)
    , ("M-k",                     addName "Focus previous"         $ windows W.focusUp)
    , ("M-m",                     addName "Focus master"           $ windows W.focusMaster)
    , ("M-S-j",                   addName "Swap next"              $ windows W.swapDown)
    , ("M-S-k",                   addName "Swap previous"          $ windows W.swapUp)
    , ("M-<Return>",              addName "Swap master"            $ windows W.swapMaster)
    , ("M-t",                     addName "Unfloat"                $ unFloat)
    ] ^++^

    subKeys "Applications"
    [ ("M-S-<Return>",            addName "Terminal emulator"      $ spawn myTerminal)
    , ("M3-<Return>",             addName "Terminal emulator"      $ spawn myTerminal)
    , ("M3-e",                    addName "Text editor"            $ spawn myEditor)
    , ("M3-w",                    addName "Web browser (minimal)"  $ spawn myBrowser)
    , ("M3-S-w",                  addName "Firefox"                $ spawn "firefox")
    , ("M3-f",                    addName "Terminal file manager"  $ spawn myFileManager)
    , ("M3-S-f",                  addName "Graphical file manager" $ spawn myGuiFileManager)
    , ("M3-z",                    addName "Zoom"                   $ spawn "zoom")
    ] ^++^

    subKeys "My Scripts"
    [ ("M-p M-p",                 addName "Edit scripts"           $ menuEditScript)
    , ("M-p M-e",                 addName "Edit configs"           $ menuEditConfig)
    , ("M-p M-c",                 addName "Change colourscheme"    $ menuChangeColourscheme)
    , ("M-p M-z",                 addName "Read PDF file"          $ menuReadPdf)
    ] ^++^

    subKeys "Multimedia Keys"
    [ ("<XF86AudioMute>",         addName "Toggle mute"            $ volumeAdjust "toggle")
    , ("<XF86AudioLowerVolume>",  addName "Decrease volume"        $ volumeAdjust "- 5%")
    , ("<XF86AudioRaiseVolume>",  addName "Increase volume"        $ volumeAdjust "+ 5%")
    , ("<XF86MonBrightnessDown>", addName "Decrease brightness"    $ brightnessAdjust "-dec 10")
    , ("<XF86MonBrightnessUp>",   addName "Increase brightness"    $ brightnessAdjust "-inc 10")
    , ("C-<F1>",                  addName "Toggle mute"            $ volumeAdjust "toggle")
    , ("C-<F2>",                  addName "Decrease volume"        $ volumeAdjust "- 5%")
    , ("C-<F3>",                  addName "Increase volume"        $ volumeAdjust "+ 5%")
    , ("C-<F11>",                 addName "Decrease brightness"    $ brightnessAdjust "-dec 10")
    , ("C-<F12>",                 addName "Increase brightness"    $ brightnessAdjust "-inc 10")
    , ("<Print>",                 addName "Take screenshot"        $ spawn myPrintScreen)
    ]

-- Keybinding to display the keybinding cheatsheet
myCheatsheetKey :: (KeyMask, KeySym)
myCheatsheetKey = (myModMask .|. shiftMask, xK_slash)
