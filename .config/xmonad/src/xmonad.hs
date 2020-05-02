import XMonad
import XMonad.Config.Desktop
import XMonad.Util.SpawnOnce

import XMonad.Util.EZConfig
import XMonad.Util.Run (hPutStrLn,spawnPipe)

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing

-- For xmobar
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import System.IO (hPutStrLn)
import qualified Data.Map as M
import Data.List (intercalate)

-- For moving workspaces
import XMonad.Actions.CycleWS

myModMask  = mod4Mask -- use the Super / Windows key as mod

-- Default applications
myTerminal     = "alacritty" -- the default terminal emulator
myTerminalApp  = myTerminal ++ " -e "
--myEditor       = "emacsclient -c"
myEditor       = myTerminalApp ++ "nvim "
myBrowser      = "qutebrowser"
myHeavyBrowser = "firefox"
myScreenshot   = "spectacle"
myLauncher     = "dmenu_run"
myMenu         = "dmenu -i -p"
myGuiFileManager = "pcmanfm"

-- Config locations
myConfigDir   = "~/.config/xmonad/src/"
myBuildScript = "~/.config/xmonad/build"
myAutostart   = myConfigDir ++ "autostart.sh"
myXmobarrc    = myConfigDir ++ "xmobarrc.hs"
myScripts = "~/.scripts/"

--------------------------------------------------------------------------------
-- MY FUNCTIONS AND SCRIPTS
--------------------------------------------------------------------------------

-- Edit a file if it exists, otherwise show an error
editIfExists :: [Char] -> [Char]
editIfExists fileName = "[ -f " ++ fileName ++ " ] \
                          \&& " ++ myEditor ++ fileName ++ " \
                          \||  notify-send \"" ++ fileName ++ " not found\""

-- Function to replace a menu script name with a shell command
runMenuScript :: [Char] -> [Char] -> [Char]
runMenuScript shell scriptName = shell++" "++myScripts++scriptName++".sh "
-- Convert strings to arguments (multiple words treated as one)
args :: [[Char]] -> [Char]
args arguments = unwords (map show arguments)

-- My scripts
editConfigs = runMenuScript "bash" "edit-configs" ++ (args [myMenu,myEditor])
editScripts = runMenuScript "bash" "edit-scripts" ++ (args [myMenu,myEditor])

--------------------------------------------------------------------------------
-- KEYBINDINGS
--------------------------------------------------------------------------------

myKeys = [ ("M-q",         spawn myBuildScript)
         , ("C-<Escape>",  spawn myLauncher)  -- launch dmenu with Super
         -- Moving workspaces
         , ("M-<Left>",    prevWS)
         , ("M-S-<Right>", nextWS)
         , ("M-<Left>",    shiftToPrev)
         , ("M-S-<Right>", shiftToNext)
         -- Application shortcuts
         , ("M-<Return>",  spawn myTerminal)
         , ("M-e",         spawn myEditor)
         , ("M-S-e",       spawn (editIfExists "~/Documents/chords/index.txt"))
         , ("M-w",         spawn myBrowser)
         , ("M-S-w",       spawn myHeavyBrowser)
         , ("M-f",         spawn myGuiFileManager)
         , ("<Print>",     spawn myScreenshot)  -- print screen
         -- Dmenu scripts
         , ("M-S-p M-S-c", spawn editConfigs)
         , ("M-S-p M-S-p", spawn editScripts)
         ]

--------------------------------------------------------------------------------
-- AESTHETICS
--------------------------------------------------------------------------------

mySpacing = spacingRaw False                -- smartBorder (border only for >1 window)
                       (Border 5 5 5 5)     -- screenBorder
                       True                 -- screenBorderEnabled
                       (Border 5 5 5 5)     -- windowBorder
                       True                 -- windowBorderEnabled

--------------------------------------------------------------------------------
-- MANAGEHOOK
--------------------------------------------------------------------------------

myManageHook = composeAll . concat $
    [ [ className =? c --> doFloat           | c <- myFloatClasses ]
    , [ title     =? t --> doFloat           | t <- myFloatTitles ]
    --, [ className =? c --> doShift "3:WWW"   | c <- browsers ]
    ]
  where myFloatClasses = ["Gimp","conky","plasmashell","vlc","Caprine", "Nitrogen"]
        myFloatTitles  = ["Whisker Menu"]
        --browsers       = ["Firefox-bin","firefox"]

--------------------------------------------------------------------------------
-- WORKSPACES
--------------------------------------------------------------------------------

myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

myCurrentWorkspacePrinter :: String -> String
myHiddenWorkspacePrinter :: String -> String
myHiddenNoWindowsWorkspacePrinter :: String -> String
myCurrentWorkspacePrinter workspaceName = "[●]"
myHiddenWorkspacePrinter workspaceName = "●"
myHiddenNoWindowsWorkspacePrinter workspaceName = "○"

main = do
    xmproc <- spawnPipe ("xmobar " ++ myXmobarrc)
    xmonad $ desktopConfig
        { modMask     = myModMask
        , terminal    = myTerminal
        , manageHook  = manageDocks <+> manageHook desktopConfig <+> myManageHook
        , layoutHook  = mySpacing $ avoidStruts $ smartBorders (layoutHook desktopConfig)
        , logHook     = dynamicLogWithPP xmobarPP
                            { ppOutput = hPutStrLn xmproc
                            , ppOrder  = \(ws:l:t:ex) -> [ws]  -- Only send workspace information
                            , ppCurrent = xmobarColor "white" "" . myCurrentWorkspacePrinter
                            , ppHidden  = xmobarColor "white" "" . myHiddenWorkspacePrinter
                            , ppHiddenNoWindows = xmobarColor "white" "" . myHiddenNoWindowsWorkspacePrinter
                            }
        , workspaces  = myWorkspaces
        , startupHook = spawnOnce myAutostart
        }
        `additionalKeysP` myKeys
