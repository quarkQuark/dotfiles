import XMonad
import XMonad.Config.Xfce

import XMonad.Util.SpawnOnce

import XMonad.Hooks.ManageDocks

import XMonad.Layout.Spacing (spacing)  -- gaps

myModMask     = mod4Mask
myTerminal    = "urxvtc"
myBorderWidth = 1

--------------------------------------------------------------------------------
-- WORKSPACES
--------------------------------------------------------------------------------

myWorkspaces = [ "⌂" , "2" , "3" , "4" ]

--------------------------------------------------------------------------------
-- LAYOUTS
--------------------------------------------------------------------------------

myLayoutHook = avoidStruts  $  layoutHook xfceConfig

--------------------------------------------------------------------------------
-- MANAGE HOOK
--------------------------------------------------------------------------------

myManageHook = composeAll
            [ className =? "Gimp"         --> doFloat
            , title     =? "Whisker Menu" --> doIgnore
            ]

--------------------------------------------------------------------------------
-- STARTUP HOOK
--------------------------------------------------------------------------------

myStartupHook = do
            spawnOnce "~/.xmonad/autostart.sh"
            spawn "xfce4-panel --restart"  -- Otherwise it's drawn over

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main = xmonad xfceConfig
            { modMask     = myModMask
            , startupHook = myStartupHook
            , terminal    = myTerminal
            , borderWidth = myBorderWidth
            , workspaces  = myWorkspaces
            , layoutHook  = myLayoutHook
            , manageHook  = myManageHook <+> manageHook xfceConfig <+> manageDocks
            }
