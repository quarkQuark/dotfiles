import XMonad
import XMonad.Config.Kde

import XMonad.Util.EZConfig

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
         -- Dmenu scripts
         , ("M-S-e",       spawn ". ~/.config/dmenu/edit-configs.sh")
         ]

--------------------------------------------------------------------------------
-- MANAGEHOOK
--------------------------------------------------------------------------------

myManageHook = composeAll . concat $
    [ [ className =? c --> doFloat           | c <- myFloats ]
    , [ title     =? t --> doFloat           | t <- myOtherFloats ]
    , [ className =? c --> doShift "🌐" | c <- browsers ]
    ]
  where myFloats      = ["Gimp","conky","plasmashell"]
        myOtherFloats = ["Whisker Menu"]
        browsers      = ["Firefox-bin","firefox"]

--------------------------------------------------------------------------------
-- WORKSPACES
--------------------------------------------------------------------------------

myWorkspaces :: [String]
myWorkspaces = ["~","2","🌐","🎵","5","6","7","8","9"]

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main = xmonad $ ewmh $ kde4Config
    { modMask     = myModMask
    , terminal    = myTerminal
    , manageHook  = manageHook kde4Config <+> myManageHook
    , layoutHook  = smartSpacing 2 $ smartBorders (layoutHook kde4Config)
    , workspaces  = myWorkspaces
    }
    `additionalKeysP` myKeys
