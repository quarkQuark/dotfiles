--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Data.List (isInfixOf)
import Prelude hiding (lookup)            -- to avoid confusing errors when mistyping Map.lookup
import XMonad                             -- standard xmonad library
import XMonad.Config.Desktop              -- default desktopConfig
import XMonad.Hooks.EwmhDesktops          -- Fixes the automatic fullscreening of applications
import XMonad.Hooks.ManageDocks           -- Manipulate and avoid docks and panels
import XMonad.Layout.NoBorders            -- Remove borders when unnecessary (smartBorders)
import XMonad.Layout.Spacing              -- Gaps around windows
import XMonad.Util.NamedActions (addDescrKeys')
import XMonad.Util.SpawnOnce (spawnOnce)  -- For running autostart only once (on login)

-- Local Modules
import MyPrograms
import MyKeys
import MyCheatsheet
import MyBar

-- I want to figure out how window decorations work, but my Haskell is not yet good enough
--import XMonad.Layout.Decoration
--import XMonad.Util.Types
--import SideDecoration

--------------------------------------------------------------------------------
-- LAYOUTHOOK
--------------------------------------------------------------------------------

myLayoutHook = avoidStruts
             $ mySpacing
             $ smartBorders
--              $ mySideDecorate  -- Messes up everything - I don't yet understand why
             ( layoutHook desktopConfig )

--------------------------------------------------------------------------------
-- AESTHETICS
--------------------------------------------------------------------------------

-- Gaps around and between windows
-- Changes only seem to apply if I log out then in again
-- Dimensions are given as (Border top bottom right left)
mySpacing = spacingRaw True             -- Only for >1 window
                       -- The bottom edge seems to look narrower than it is
                       (Border 0 15 10 10) -- Size of screen edge gaps
                       True             -- Enable screen edge gaps
                       (Border 5 5 5 5) -- Size of window gaps
                       True             -- Enable window gaps

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColour, myFocusedBorderColour :: String
myNormalBorderColour = "#111111"
myFocusedBorderColour = "#268bd2"

--mySideDecorationTheme :: Theme
--mySideDecorationTheme = def
--mySideDecorate :: Eq a => l a -> ModifiedLayout (Decoration SideDecoration DefaultShrinker) l a
--mySideDecorate = decoration shrinkText mySideDecorationTheme (SideDecoration L)

--------------------------------------------------------------------------------
-- MANAGEHOOK
-- special rules based on window types
--------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    -- Windows to automatically float
    [ [ className =? c                                    --> doFloat | c <- myFloatClasses ]
    , [ title     =? t                                    --> doFloat | t <- myFloatTitles ]
    , [ className =? "zoom" <&&> fmap (isInfixOf z) title --> doFloat | z <- myZoomFloats ]
    , [ className =? "zoom" <&&> title =? "zoom"          --> doFloat ] -- Zoom notification popups
    ]
  -- To find a window class or title, run xprop in a terminal, then click on it
  where myFloatClasses = ["Gimp", "conky", "plasmashell", "vlc", "Nitrogen", "Tint2conf"]
        myFloatTitles  = ["Whisker Menu"]
        -- This allows annoying classes such as "Participants (n)" where n is the number of people
        myZoomFloats  = ["Chat", "Participants", "Rooms"] -- Currently untested for breakout rooms

--------------------------------------------------------------------------------
-- WORKSPACES
--------------------------------------------------------------------------------

-- My workspaces are currently just numbers
myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
    barProc <- mySpawnBar     -- Start myBar and return a handle
    spawn "pkill -o taffybar" -- Kill oldest taffybar instance (move to M-q binding?)

    -- Applies this config file over the default config for desktop use
    xmonad
        -- Increased compliance with the Extended Window Manager Hints standard
        $ ewmh
        -- Add keybindings in such a way as to allow viewing a cheatsheet with M-?
        $ addDescrKeys' (myCheatsheetKey, myCheatsheet) myKeys
        $ myConfig barProc

myConfig barProc = desktopConfig
        -- Variables
        { modMask  = myModMask
        , terminal = myTerminal
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColour
        , focusedBorderColor = myFocusedBorderColour
        -- Hooks
        , manageHook  = manageDocks <+> manageHook desktopConfig <+> myManageHook
        , layoutHook  = myLayoutHook
        , logHook     = myLogHook barProc
        , workspaces  = myWorkspaces
        , startupHook = do
            spawnOnce myBarAutostart
            spawnOnce myAutostart
        }
