--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Data.List.Split (chunksOf)         -- Requires the package 'split'
import Prelude hiding (lookup)            -- to avoid confusing errors when mistyping Map.lookup
import System.Exit (exitSuccess)
import System.IO
import Test.FitSpec.PrettyPrint (columns) -- Requires the package 'fitspec'
import XMonad                             -- standard xmonad library
import XMonad.Config.Desktop              -- default desktopConfig
import XMonad.Hooks.DynamicLog            -- Customising the logHook (sends data to xmobar)
import XMonad.Hooks.EwmhDesktops          -- Fixes the automatic fullscreening of applications
import XMonad.Hooks.ManageDocks           -- Manipulate and avoid docks and panels
import XMonad.Layout.NoBorders            -- Remove borders when unnecessary (smartBorders)
import XMonad.Layout.Spacing              -- Gaps around windows
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig               -- Simpler keybinding syntax
import XMonad.Util.NamedActions           -- AwesomeWM-style keybinding syntax
import XMonad.Util.Run                    -- Start and send information to processes
import XMonad.Util.SpawnOnce              -- For running autostart only once (on login)

-- I want to figure out how window decorations work, but my Haskell is not yet good enough
import XMonad.Layout.Decoration
import XMonad.Util.Types
import SideDecoration

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
myFileManager    = myTerminal ++ " -e ranger "
myGuiFileManager = "pcmanfm"
myPdfReader      = "zathura"
myPrintScreen    = "spectacle"
myBar            = Tint2

myMenu :: String
myMenu     = "rofi -dmenu -i -p"  -- For scripts that require user input

-- Config locations
myConfigDir   = "~/.config/xmonad/src/"       -- XMonad-related config
myBuildScript = "~/.config/xmonad/build"      -- Script to recompile and restart xmonad
myAutostart   = myConfigDir ++ "autostart.sh" -- Script to run on login
rofiTheme theme = "~/.config/rofi/themes/" ++ theme ++ ".rasi" -- Rofi theme directory

--------------------------------------------------------------------------------
-- TYPES, FUNCTIONS AND SCRIPTS
--------------------------------------------------------------------------------

-- Status bar management
data Bar = Taffybar | XMobar | Tint2
myBar :: Bar
spawnBarProcCmd, myBarAutostart :: Bar -> String

-- If I want it to be able to read myLogHook
spawnBarProcCmd XMobar = "xmobar " ++ myConfigDir ++ "xmobarrc.hs"
spawnBarProcCmd other  = ""

-- Anything else bar-dependent
myBarAutostart XMobar   = "stalonetray --config " ++ myConfigDir ++ "stalonetrayrc"
myBarAutostart Tint2    = "tint2 -c ~/.config/tint2/xmonad.tint2rc"
myBarAutostart Taffybar = "status-notifier-watcher && taffybar" -- From status-notifier-item

-- Convert multiword strings to arguments (concatenate with delimiters)
-- This makes sure my shell scripts correctly interpret their arguments
args :: String -> [String] -> String
args command arguments = command ++ " " ++ unwords (map show arguments)

--------------------------------------------------------------------------------
-- KEYBINDINGS
--------------------------------------------------------------------------------
-- M = M1 is Super, which I have also set to space when held down
-- M3 is Hyper, which I have set to Caps Lock
-- C-Esc is Super tapped on its own

myKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myKeys conf = let

    subKeys name list = subtitle name : mkNamedKeymap conf list

    -- Abbreviations for certain actions
    menuEditScript         = spawn $ args "menu-edit-script" [myMenu,myEditor]
    menuEditConfig         = spawn $ args "menu-edit-config" [myMenu,myEditor]
    menuChangeColourscheme = spawn $ args "menu-edit-colourscheme" [myMenu]
    menuReadPdf            = spawn $ args "menu-read-pdf" [myMenu,myPdfReader]

    viewScreen s          = screenWorkspace s >>= flip whenJust (windows . W.view)
    shiftScreen s         = screenWorkspace s >>= flip whenJust (windows . W.shift)
    unFloat               = withFocused $ windows . W.sink

    volumeAdjust "toggle" = spawn "adjust-volume toggle"
    volumeAdjust value    = spawn $ args "adjust-volume" $ words value

    brightnessAdjust perc = spawn
        $ "xbacklight " ++ perc ++ " && notify-send \"Brightness `xbacklight -get`%\""

    in

    subKeys "Core"
    [ ("M-S-q",                   addName "Quit XMonad (logout)"   $ io exitSuccess)
    , ("M-q",                     addName "Recompile & restart"    $ spawn myBuildScript)
    , ("C-<Escape>",              addName "Application launcher"   $ spawn "appmenu")
    , ("M-S-c",                   addName "Close window"           $ kill)
    ] ^++^

    subKeys "Screens" (
    [("M-"++key,                  addName ("Focus screen "++show sc)   $ viewScreen sc)
        | (key,sc) <- zip ["w","e","r"] [0..]
    ] ^++^
    [("M-S-"++key,                addName ("Send to screen "++show sc) $ shiftScreen sc)
        | (key,sc) <- zip ["w","e","r"] [0..]
    ]) ^++^

    subKeys "Workspaces" (
    --[ ("M-u",                     addName "View next"              $ )
    --, ("M-i,",                    addName "View previous"          $ )
    --, ("M-S-u",                   addName "Send to next"           $ )
    --, ("M-S-i",                   addName "Send to previous"       $ )
    --] ^++^
    [ ("M-"++show key,            addName ("View workspace "++i)    $ windows $ W.greedyView i)
        | (key,i) <- zip [1..9] (XMonad.workspaces conf)
    ] ^++^
    [ ("M-S-"++show key,          addName ("Send to workspace "++i) $ windows $ W.shift i)
        | (key,i) <- zip [1..9] (XMonad.workspaces conf)
    ]) ^++^

    subKeys "Layouts"
    [ ("M-S-f",                   addName "Toggle fullscreen"      $ sendMessage ToggleStruts)
    , ("M-h",                     addName "Shrink master"          $ sendMessage Shrink)
    , ("M-l",                     addName "Expand master"          $ sendMessage Expand)
    , ("M-,",                     addName "Inc master windows"     $ sendMessage $ IncMasterN 1)
    , ("M-.",                     addName "Dec master windows"     $ sendMessage $ IncMasterN (-1))
    , ("M3-<Space>",              addName "Next layout"            $ sendMessage NextLayout)
    ] ^++^

    subKeys "Windows"
    [ ("M-<Tab>",                 addName "Focus next"             $ windows W.focusDown)
    , ("M-S-<Tab>",               addName "Focus previous"         $ windows W.focusUp)
    , ("M-j",                     addName "Focus next"             $ windows W.focusDown)
    , ("M-k",                     addName "Focus previous"         $ windows W.focusUp)
    , ("M-m",                     addName "Focus master"           $ windows W.focusMaster)
    , ("M-S-j",                   addName "Swap next"              $ windows W.swapDown)
    , ("M-S-k",                   addName "Swap previous"          $ windows W.swapUp)
    , ("M-<Return>",              addName "Swap master"            $ windows W.swapMaster)
    , ("M-t",                     addName "Unfloat"                $ unFloat)
    ] ^++^

    subKeys "Applications"
    [ ("M-S-<Return>",            addName "Terminal emulator"      $ spawn myTerminal)
    , ("M3-<Return>",             addName "Terminal emulator"      $ spawn myTerminal)
    , ("M3-e",                    addName "Text editor"            $ spawn myEditor)
    , ("M3-w",                    addName "Web browser (minimal)"  $ spawn myBrowser)
    , ("M3-S-w",                  addName "Firefox"                $ spawn "firefox")
    , ("M3-f",                    addName "Terminal file manager"  $ spawn myFileManager)
    , ("M3-S-f",                  addName "Graphical file manager" $ spawn myGuiFileManager)
    , ("M3-z",                    addName "Zoom"                   $ spawn "zoom")
    ] ^++^

    subKeys "My Scripts"
    [ ("M-p M-p",                 addName "Edit scripts"           $ menuEditScript)
    , ("M-p M-e",                 addName "Edit configs"           $ menuEditConfig)
    , ("M-p M-c",                 addName "Change colourscheme"    $ menuChangeColourscheme)
    , ("M-p M-z",                 addName "Read PDF file"          $ menuReadPdf)
    ] ^++^

    subKeys "Multimedia Keys"
    [ ("<XF86AudioMute>",         addName "Toggle mute"            $ volumeAdjust "toggle")
    , ("<XF86AudioLowerVolume>",  addName "Decrease volume"        $ volumeAdjust "- 5%")
    , ("<XF86AudioRaiseVolume>",  addName "Increase volume"        $ volumeAdjust "+ 5%")
    , ("<XF86MonBrightnessDown>", addName "Decrease brightness"    $ brightnessAdjust "-dec 10")
    , ("<XF86MonBrightnessUp>",   addName "Increase brightness"    $ brightnessAdjust "-inc 10")
    , ("C-<F1>",                  addName "Toggle mute"            $ volumeAdjust "toggle")
    , ("C-<F2>",                  addName "Decrease volume"        $ volumeAdjust "- 5%")
    , ("C-<F3>",                  addName "Increase volume"        $ volumeAdjust "+ 5%")
    , ("C-<F11>",                 addName "Decrease brightness"    $ brightnessAdjust "-dec 10")
    , ("C-<F12>",                 addName "Increase brightness"    $ brightnessAdjust "-inc 10")
    , ("<Print>",                 addName "Take screenshot"        $ spawn myPrintScreen)
    ]

-- Keybinding to display the keybinding cheatsheet
myCheatsheetKey :: (KeyMask, KeySym)
myCheatsheetKey = (myModMask .|. shiftMask, xK_slash)

-- Number of colomns with with which to display the cheatsheet
myCheatsheetCols :: Int
myCheatsheetCols = 3

-- Format the keybindings so they can be sent to the display
formatList :: [String] -> String
formatList list = columns "SeparatorPlaceholder" -- Normalise column widths -> Table
                $ map unlines -- Connect the sublists with line breals -> [column1,column2,...]
                $ chunksOf (myCheatsheetRows (list))
                $ list -- The list to be formatted

        where rowsFromColumns list nCol = 1 + length list `div` nCol
              myCheatsheetRows list = rowsFromColumns list myCheatsheetCols

-- How to display the cheatsheet (adapted from Ethan Schoonover's config)
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings myKeyList = addName "Show Keybindings" $ io $ do
    handle <- spawnPipe "dzen2-display-cheatsheet"
    hPutStrLn handle "TitlePlaceholder\n" -- Replaced in the script
    hPutStrLn handle $ formatList (showKm myKeyList)
    hClose handle
    return ()

--------------------------------------------------------------------------------
-- LAYOUTHOOK
--------------------------------------------------------------------------------

myLayoutHook = avoidStruts
             $ mySpacing
             $ smartBorders
--              $ mySideDecorate  -- Messes up everythin. I don't know why (too complicated for me to understamd
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

mySideDecorationTheme :: Theme
mySideDecorationTheme = def

mySideDecorate :: Eq a => l a -> ModifiedLayout (Decoration SideDecoration DefaultShrinker) l a
mySideDecorate = decoration shrinkText mySideDecorationTheme (SideDecoration L)

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
myLogHook barProc = dynamicLogWithPP xmobarPP
        -- Write to bar instead of stdout
        { ppOutput          = hPutStrLn barProc
        -- How to order the different sections of the log
        , ppOrder           = \(workspace:layout:title:extras)
                            -> [workspace,layout]
        -- Separator between different sections of the log
        , ppSep             = "  "
        -- Format the workspace information
        , ppCurrent         = xmobarColor "white" "" . myCurrentWsSymbol
        , ppHidden          = xmobarColor "white" "" . myHiddenWsSymbol
        , ppHiddenNoWindows = xmobarColor "white" "" . myEmptyWsSymbol
        }

--------------------------------------------------------------------------------
-- MANAGEHOOK
-- special rules based on window types
--------------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    -- Windows to automatically float
    [ [ className =? c --> doFloat                 | c <- myFloatClasses ]
    , [ title     =? t --> doFloat                 | t <- myFloatTitles ]
    ]
  -- To find a window class or title, run xprop in a terminal, then click on it
  where myFloatClasses = [ "Gimp", "conky", "plasmashell", "vlc", "Caprine", "Nitrogen"
                         , "Tint2conf"]
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
main :: IO ()
main = do
    barProc <- spawnPipe (spawnBarProcCmd myBar)  -- Start myBar and return a handle
    spawn "pkill -o taffybar" -- Kill oldest taffybar instance (move to M-q binding?)

    -- Applies this config file over the default config for desktop use
    xmonad
        -- Increased compliance with the Extended Window Manager Hints standard
        $ ewmh
        -- Use my config, with the process handle for xmobar
        -- Add keybindings in such a way as to allow viewing a cheatsheet with M-?
        -- showKeybindings is the script used to display them
        -- The prime shows that we are not merging with the default keys
        $ addDescrKeys' (myCheatsheetKey, showKeybindings) myKeys
        $ myConfig barProc

-- Adding all of my stuff to the default desktop config
myConfig barProc = desktopConfig
        -- Variables
        { modMask  = myModMask
        , terminal = myTerminal
        -- Borders
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColour
        , focusedBorderColor = myFocusedBorderColour
        -- Hooks
        , manageHook  = manageDocks <+> manageHook desktopConfig <+> myManageHook
        , layoutHook  = myLayoutHook
        , logHook     = myLogHook barProc
        , workspaces  = myWorkspaces
        , startupHook = do
            spawnOnce (myBarAutostart myBar)
            spawnOnce myAutostart
        }
