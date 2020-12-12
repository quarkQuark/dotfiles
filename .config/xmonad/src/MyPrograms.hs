module MyPrograms
where

import XMonad

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

data Bar = Taffybar | XMobar | Tint2
myBar :: Bar
myBar = XMobar

myMenu :: String
myMenu = "rofi -dmenu -i -p"  -- For scripts that require user input

-- Config locations
myConfigDir   = "~/.config/xmonad/src/"       -- XMonad-related config
myBuildScript = "~/.config/xmonad/build"      -- Script to recompile and restart xmonad
myAutostart   = myConfigDir ++ "autostart.sh" -- Script to run on login
rofiTheme theme = "~/.config/rofi/themes/" ++ theme ++ ".rasi" -- Rofi theme directory
