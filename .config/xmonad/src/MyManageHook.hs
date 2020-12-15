module MyManageHook
(myManageHook)
where

import Data.List (isInfixOf)
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks

-- To find a window class or title, run xprop in a terminal, then click on it
manageSpecific :: ManageHook
manageSpecific = composeAll . concat $
    -- Windows to automatically float
    [ [ className =? c                                    --> doFloat | c <- myFloatClasses ]
    , [ title     =? t                                    --> doFloat | t <- myFloatTitles ]
    , [ className =? "zoom" <&&> fmap (isInfixOf z) title --> doFloat | z <- myZoomFloats ]
    , [ className =? "zoom" <&&> title =? "zoom"          --> doFloat ] -- Zoom notification popups
    ]
    where
        myFloatClasses = ["Gimp", "conky", "plasmashell", "vlc", "Nitrogen", "Tint2conf"]
        myFloatTitles  = ["Whisker Menu"]
        -- This allows annoying classes such as "Participants (n)" where n is the number of people
        myZoomFloats  = ["Chat", "Participants", "Rooms"] -- Currently untested for breakout rooms

myManageHook :: ManageHook
myManageHook = manageDocks
           <+> manageSpecific
           <+> manageHook desktopConfig
