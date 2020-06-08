--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

-- Foundations
import XMonad -- standard xmonad library
import XMonad.Config.Desktop -- default desktopConfig

-- More fine-grained control over window placement
-- Must be qualified, as otherwise it clashes with XMonad.workspaces
import qualified XMonad.StackSet as W

-- Used to make sure my autostart script is run only on login
import XMonad.Util.SpawnOnce

-- Simplifies the syntax for defining keybindings
import XMonad.Util.EZConfig

-- Removes window borders if they aren't needed
import XMonad.Layout.NoBorders (smartBorders)
-- Allow gaps to be displayed around windows, for aesthetic purposes
import XMonad.Layout.Spacing
-- Fixes automatic fullscreening of applications
import XMonad.Hooks.EwmhDesktops

--------------------------------------------------------------------------------

-- For xmobar

-- For starting and sending information to the xmobar status bar
import XMonad.Util.Run (spawnPipe,hPutStrLn)
-- Allows us to customise the logHook that sends information to xmobar
import XMonad.Hooks.DynamicLog
-- Provides tools to manipulate docks and panels, and to avoid overlapping them
import XMonad.Hooks.ManageDocks

--------------------------------------------------------------------------------
-- VARIABLES AND DEFAULT PROGRAMS
--------------------------------------------------------------------------------

-- The modifier key to be used for most keybindings
-- I have it set to super (the Windows key)
myModMask :: KeyMask
myModMask  = mod4Mask

-- Default applications
myTerminal       = "alacritty"
myEditor         = myTerminal ++ " -e nvim "
myBrowser        = "qutebrowser"
myHeavyBrowser   = "firefox"
myGuiFileManager = "pcmanfm"
myPdfReader      = "zathura"
myPrintScreen    = "spectacle"
myStatusBar      = "xmobar " ++ myXmobarRC

-- Command to use for the various menus
--  myLauncher is the menu for opening applications
--  myMenu is used for displaying user-generated menus (my shell menu scripts)
-- dmenu is a much simpler option, but with less eye-candy
--myLauncher = "dmenu_run"
--myMenu     = "dmenu -i -p"
-- rofi looks much nicer, but is less minimal and the default theme is ugly
myLauncher = "rofi -show drun -theme " ++ rofiTheme "blurry-icons-centre"
myMenu     = "rofi -dmenu -i -p"

--------------------------------------------------------------------------------

-- Config locations

-- Directory for storing xmonad-related config files
myConfigDir   = "~/.config/xmonad/src/"
-- The script run to recompile xmonad after config changes
myBuildScript = "~/.config/xmonad/build"
-- Programs to start automatically on login
myAutostart   = myConfigDir ++ "autostart.sh"
-- Config for the xmobar status bar
myXmobarRC    = myConfigDir ++ "xmobarrc.hs"
-- Directory that contains all my rofi themes, for the rofi menu program
rofiTheme theme = "~/.config/rofi/themes/" ++ theme ++ ".rasi"

--------------------------------------------------------------------------------
-- MY FUNCTIONS AND SCRIPTS
--------------------------------------------------------------------------------

-- Edit a file if it exists, otherwise show an error
-- This basically just concatenates togather a simple shell script
editIfExists :: [Char] -> [Char]
editIfExists fileName = "[ -f " ++ fileName ++ " ] \
                          \&& " ++ myEditor ++ fileName ++ " \
                          \||  notify-send \"" ++ fileName ++ " not found\""

-- Convert multiword strings to arguments (concatenate with delimiters)
-- This makes sure my shell scripts correctly interpret their arguments
args :: [[Char]] -> [Char]
args arguments = " " ++ unwords (map show arguments)

--------------------------------------------------------------------------------
-- KEYBINDINGS
--------------------------------------------------------------------------------

-- spawn runs a string on my system shell
myKeys = [ ("M-q",          spawn myBuildScript) -- recompile xmonad
         , ("C-<Escape>",   spawn myLauncher)  -- launch dmenu with Super
         -- Toggle fullscreen
         , ("M-S-f",        sendMessage ToggleStruts)
         -- Application shortcuts
         , ("M-e",          spawn myEditor)
         , ("M-S-e",        spawn (editIfExists "Chords/index.txt"))
         , ("M-w",          spawn myBrowser)
         , ("M-S-w",        spawn myHeavyBrowser)
         , ("M-f",          spawn myGuiFileManager)
         , ("M-z",          spawn "zoom")
         , ("<Print>",      spawn myPrintScreen)
         -- Menu scripts
         , ("M-S-p M-S-p",  spawn ("menu-edit-script" ++ (args[myMenu,myEditor])))
         , ("M-S-p M-S-e",  spawn ("menu-edit-config" ++ (args[myMenu,myEditor])))
         , ("M-S-p M-S-c",  spawn ("menu-change-colourscheme" ++ (args[myMenu])))
         , ("M-S-p M-S-z",  spawn ("menu-read-pdf" ++ (args[myMenu,myPdfReader])))
         ]

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

myBorderWidth = 2
myNormalBorderColour = "#111111"
myFocusedBorderColour = "#268bd2"

--------------------------------------------------------------------------------
-- LOGHOOK
-- The information to send to xmobar, through the handle we defined earlier
--------------------------------------------------------------------------------

-- Symbols for displaying workspaces in xmobar

-- Must be functions, as it expects a different symbol for each
myCurrentWsSymbol workspaceName = "[●]" -- The workspace currently active
myHiddenWsSymbol  workspaceName =  "●"  -- Workspaces with open windows
myEmptyWsSymbol   workspaceName =  "○"  -- Workspaces with no windows

-- bar points to the status bar's process handle
-- XMonad.Hooks.DynamicLog (dynamicLogWithPP) allows us to format the output
-- XMonad.Hooks.DynamicLog (xmobarPP) gives us some defaults
myLogHook bar = dynamicLogWithPP xmobarPP
        -- Formatting to apply to the entire log, after all the other formatting
        { ppOutput          = hPutStrLn bar
        -- How to order the different sections of the log
        -- I only want to display the various workspaces
        , ppOrder           = \(workspace:layout:title:extras)
                            -> [workspace] -- Only send workspace information
        -- Format the workspace information
        , ppCurrent         = xmobarColor "white" "" . myCurrentWsSymbol
        , ppHidden          = xmobarColor "white" "" . myHiddenWsSymbol
        , ppHiddenNoWindows = xmobarColor "white" "" . myEmptyWsSymbol
        }

--------------------------------------------------------------------------------
-- MANAGEHOOK
-- special rules based on window types
--------------------------------------------------------------------------------

myManageHook = composeAll . concat $
    -- Windows to automatically float
    [ [ className =? c --> doFloat           | c <- myFloatClasses ]
    , [ title     =? t --> doFloat           | t <- myFloatTitles ]
    ]
  -- To find a window class or title, run xprop in a terminal, then click on it
  where myFloatClasses = ["Gimp","conky","plasmashell","vlc","Caprine", "Nitrogen"]
        myFloatTitles  = ["Whisker Menu"]

--------------------------------------------------------------------------------
-- WORKSPACES
--------------------------------------------------------------------------------

-- My workspaces are currently just numbers
myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

--------------------------------------------------------------------------------
-- MAIN
-- putting it all together
--------------------------------------------------------------------------------

-- This is the part that is actually run as a window manager
main = do
    -- spawnPipe starts xmobar and returns a handle - named xmproc - for input
    xmproc <- spawnPipe myStatusBar

    -- Applies this config file over the default config for desktop use
    xmonad
        -- Increased compliance with the Extended Window Manager Hints standard
        $ ewmh
        -- Use my config, with the process handle for xmobar
        $ myConfig xmproc

-- Adding all of my stuff to the default desktop config
myConfig bar = desktopConfig
        -- Variables
        { modMask  = myModMask
        , terminal = myTerminal
        -- Borders
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColour
        , focusedBorderColor = myFocusedBorderColour
        -- Hooks
        , manageHook  = manageDocks <+> manageHook desktopConfig <+> myManageHook
        , layoutHook  = avoidStruts $ mySpacing $ smartBorders (layoutHook desktopConfig)
        , logHook     = myLogHook bar
        , workspaces  = myWorkspaces
        , startupHook = spawnOnce myAutostart
        }
        `additionalKeysP` myKeys
