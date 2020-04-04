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

-- For moving workspaces
import XMonad.Actions.CycleWS

myModMask  = mod4Mask -- use the Super / Windows key as mod
myTerminal = "urxvtc" -- the default terminal emulator
myEditor   = "emacsclient -c" -- needs to be changed in dmenu script files as well

--------------------------------------------------------------------------------
-- KEYBINDINGS
--------------------------------------------------------------------------------

myKeys = [ ("C-<Escape>", spawn "dmenu_run")  -- launch dmenu with Super
         -- Moving workspaces
         , ("M-<Left>",    prevWS)
         , ("M-S-<Right>", nextWS)
         , ("M-<Left>",    shiftToPrev)
         , ("M-S-<Right>", shiftToNext)
         -- Application shortcuts
         , ("M-<Return>",  spawn myTerminal)
         , ("M-e",         spawn myEditor)
         , ("M-w",         spawn "qutebrowser")
         , ("<Print>",     spawn "spectacle")  -- print screen
         -- Dmenu scripts
         , ("M-S-e",       spawn ". ~/.config/dmenu/edit-configs.sh")
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
  where myFloatClasses = ["Gimp","conky","plasmashell","vlc","Caprine"]
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

main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/config/xmobarrc.hs"
    xmonad $ desktopConfig
        { modMask     = myModMask
        , terminal    = myTerminal
        , manageHook  = manageDocks <+> manageHook desktopConfig <+> myManageHook
        , layoutHook  = mySpacing $ avoidStruts $ smartBorders (layoutHook desktopConfig)
        , logHook     = dynamicLogWithPP xmobarPP
                            { ppOutput = hPutStrLn xmproc
                            , ppCurrent = xmobarColor "#ffffff" "" . wrap "[" "]"
                            , ppVisible = xmobarColor "#eeeeee" ""
                            , ppTitle = xmobarColor "#ffffff" ""
                            }
        , workspaces  = myWorkspaces
        , startupHook = spawnOnce "~/.xmonad/config/autostart.sh"
        }
        `additionalKeysP` myKeys
