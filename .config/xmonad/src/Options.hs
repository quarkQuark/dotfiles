module Options where

import XMonad

-- The modifier key to be used for most keybindings
-- I have it set to super (the Windows key)
myModMask :: KeyMask
myModMask  = mod4Mask

--------------------------------------------------------------------------------
-- APPLICATIONS

myTerminal       = "alacritty"
myEditor         = myTerminal ++ " -e nvim "
myBrowser        = "qutebrowser"
myFileManager    = myTerminal ++ " -e ranger "
myGuiFileManager = "pcmanfm"
myPdfReader      = "zathura"
myPrintScreen    = "spectacle"

data Bar = Taffybar | XMobar | Tint2
myBar :: Bar
myBar = XMobar

myMenu :: String
myMenu = "rofi -dmenu -i -p"  -- For scripts that require user input

--------------------------------------------------------------------------------
-- FILEPATHS

myConfigDir   = "~/.config/xmonad/src/"       -- XMonad-related config
myBuildScript = "~/.config/xmonad/build"      -- Script to recompile and restart xmonad
myAutostart   = myConfigDir ++ "autostart.sh" -- Script to run on login

--------------------------------------------------------------------------------
-- THEME

myFont = "xft:Ubuntu Nerd Font:size=10"
rofiTheme theme = "~/.config/rofi/themes/" ++ theme ++ ".rasi" -- Rofi theme directory

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColour, myFocusedBorderColour :: String
myNormalBorderColour = "#111111"
myFocusedBorderColour = "#268bd2"

-- Tab colours copied from DistroTube's config (at gitlab/dwt1)
myTabActiveColour      = "#46D9FF"
myTabInactiveColour       = "#313846"
myTabActiveBorderColour   = "#46D9FF"
myTabInactiveBorderColour = "#282C34"
myTabActiveTextColour     = "#282C34"
myTabInactiveTextColour   = "#D0D0D0"
