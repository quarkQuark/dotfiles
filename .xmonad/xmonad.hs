import XMonad
import XMonad.Config.Kde

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacing)

-- For KDE workspace widget to work
import XMonad.Hooks.EwmhDesktops (ewmh)

-- For shifting and floating windows
import qualified XMonad.StackSet as W

-- For moving workspaces
import XMonad.Actions.CycleWS

myModMask  = mod4Mask -- use the Super / Windows key as mod
myTerminal = "urxvtc" -- the default terminal emulator

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
         , ("M-e",         spawn "emacsclient -c")
         , ("M-w",         spawn "qutebrowser")
         -- Dmenu scripts
         , ("M-C-e",       spawn ". ~/.config/dmenu/edit-configs.sh")
         ]

--------------------------------------------------------------------------------
-- MANAGEHOOK
--------------------------------------------------------------------------------

myManageHook = composeAll . concat $
    [ [ className =? c --> doFloat           | c <- myFloats ]
    , [ title     =? t --> doFloat           | t <- myOtherFloats ]
    , [ className =? c --> doF (W.shift "2") | c <- webApps ]
    ]
  where myFloats      = ["Gimp","conky","plasmashell"]
        myOtherFloats = ["Whisker Menu"]
        webApps       = []

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main = xmonad $ ewmh $ kde4Config
    { modMask     = myModMask
    , terminal    = myTerminal
    , manageHook  = manageHook kde4Config <+> myManageHook
    , layoutHook  = smartSpacing 2 $ smartBorders (layoutHook kde4Config)
    }
    `additionalKeysP` myKeys
