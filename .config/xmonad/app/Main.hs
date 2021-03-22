import XMonad                             -- standard xmonad library
import XMonad.Config.Desktop              -- default desktopConfig
import XMonad.Hooks.EwmhDesktops          -- Fixes the automatic fullscreening of applications
import XMonad.Util.NamedActions (addDescrKeys')
import XMonad.Util.SpawnOnce (spawnOnce)  -- For running autostart only once (on login)

import Options
import MyKeys
import MyCheatsheet
import MyBar
import MyLayoutHook
import MyManageHook

-- I want to figure out how window decorations work, but my Haskell is not yet good enough
--import XMonad.Layout.Decoration
--import XMonad.Util.Types
--import SideDecoration

--mySideDecorationTheme :: Theme
--mySideDecorationTheme = def
--mySideDecorate :: Eq a => l a -> ModifiedLayout (Decoration SideDecoration DefaultShrinker) l a
--mySideDecorate = decoration shrinkText mySideDecorationTheme (SideDecoration L)

-- My workspaces are currently just numbers
myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

main :: IO ()
main = do
    barProc <- spawnBarWithHandle  -- Start myBar and return a handle
    spawn "pkill -o taffybar" -- Kill oldest taffybar instance (move to M-q binding?)

    -- Applies this config file over the default config for desktop use
    xmonad
        -- Increased compliance with the Extended Window Manager Hints standard
        $ ewmh
        -- Add keybindings in such a way as to allow viewing a cheatsheet with M-?
        $ addDescrKeys' (myCheatsheetKey, myCheatsheet) myKeys
        $ myConfig barProc
  where
    myConfig barProc = desktopConfig
            { modMask            = myModMask
            , terminal           = myTerminal
            , borderWidth        = myBorderWidth
            , normalBorderColor  = myNormalBorderColour
            , focusedBorderColor = myFocusedBorderColour
            , manageHook         = myManageHook
            , layoutHook         = myLayoutHook
            , logHook            = myLogHook barProc
            , workspaces         = myWorkspaces
            , startupHook        = do spawnOnce myBarAutostart
                                      spawnOnce myAutostart
            }
