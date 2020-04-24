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

-- For moving workspaces
import XMonad.Actions.CycleWS

myModMask  = mod4Mask -- use the Super / Windows key as mod
myTerminal = "urxvtc" -- the default terminal emulator
--myEditor   = "emacsclient -c"
myEditor   = myTerminal ++ " -e nvim "
myMenuScriptPath = "~/.config/dmenu/"

-- Function to execute shell scripts from myMenuScriptPath
myMenuScript :: [Char] -> [Char]
myMenuScript scriptName = ". " ++ myMenuScriptPath ++ scriptName ++ ".sh "

--------------------------------------------------------------------------------
-- KEYBINDINGS
--------------------------------------------------------------------------------

myKeys = [ ("M-q",         spawn "~/.config/xmonad/build")
         , ("C-<Escape>",  spawn "dmenu_run")  -- launch dmenu with Super
         -- Moving workspaces
         , ("M-<Left>",    prevWS)
         , ("M-S-<Right>", nextWS)
         , ("M-<Left>",    shiftToPrev)
         , ("M-S-<Right>", shiftToNext)
         -- Application shortcuts
         , ("M-<Return>",  spawn myTerminal)
         , ("M-e",         spawn myEditor)
         , ("M-S-e",       spawn (myEditor ++ "~/Documents/chords/index.txt"))
         , ("M-w",         spawn "qutebrowser")
         , ("<Print>",     spawn "spectacle")  -- print screen
         -- Dmenu scripts
         , ("M-S-p M-S-c", spawn ((myMenuScript "edit-configs") ++ myEditor))
         , ("M-S-p M-S-p", spawn ((myMenuScript "edit-scripts") ++ myEditor))
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
-- ◦◯◦○
main = do
    xmproc <- spawnPipe "xmobar ~/.config/xmonad/src/xmobarrc.hs"
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
        , startupHook = spawnOnce "~/.config/xmonad/src/autostart.sh"
        }
        `additionalKeysP` myKeys
