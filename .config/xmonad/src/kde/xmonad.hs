import XMonad
import XMonad.Config.Kde (kde4Config)

import XMonad.Util.EZConfig
import XMonad.Util.Run (hPutStrLn,spawnPipe)

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacing)

-- For xmobar
import XMonad.Hooks.DynamicLog

import XMonad.Hooks.ManageDocks

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
         , ("M-e",         spawn myEditor)
         , ("M-w",         spawn "qutebrowser")
         , ("<Print>",     spawn "spectacle")  -- print screen
         -- Dmenu scripts
         , ("M-S-e",       spawn ". ~/.config/dmenu/edit-configs.sh")
         ]

--------------------------------------------------------------------------------
-- MANAGEHOOK
--------------------------------------------------------------------------------

myManageHook = composeAll . concat $
    [ [ className =? c --> doFloat           | c <- myFloatClasses ]
    , [ title     =? t --> doFloat           | t <- myFloatTitles ]
    , [ className =? c --> doShift "3:WWW"   | c <- browsers ]
    ]
  where myFloatClasses = ["Gimp","conky","plasmashell","vlc","Caprine"]
        myFloatTitles  = ["Whisker Menu"]
        browsers       = ["Firefox-bin","firefox"]

--------------------------------------------------------------------------------
-- WORKSPACES
--------------------------------------------------------------------------------

myWorkspaces :: [String]
myWorkspaces = ["1:HOME","2:WORK","3:WWW","4","5","6","7","8:CHORDS","9:CHAT"]

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"

    xmonad $ kde4Config
        { modMask     = myModMask
        , terminal    = myTerminal
        , manageHook  = manageDocks <+> manageHook kde4Config <+> myManageHook
        , layoutHook  = avoidStruts $ smartSpacing 2 $ smartBorders (layoutHook kde4Config)
        , workspaces  = myWorkspaces
        , logHook     = dynamicLogWithPP xmobarPP
                            { ppOutput  = hPutStrLn xmproc
                            , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace
                            , ppHidden          = xmobarColor "#82AAFF" "" -- Other workspaces with windows
                            , ppHiddenNoWindows = xmobarColor "#F07178" "" -- Empty workspaces
                            , ppUrgent  = xmobarColor "#C45500" "" . wrap "!" "!" -- Urgent workspaces
                            }
        }
        `additionalKeysP` myKeys
