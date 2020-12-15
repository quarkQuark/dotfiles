module MyBar
(spawnBarWithHandle, myBarAutostart, myLogHook)
where

import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import Options

-- Shell commands

-- Spawn the bar, returning its handle
spawnBarWithHandle :: IO (Handle)
spawnBarWithHandle
    | myBar == XMobar = spawnPipe $ "xmobar " ++ myXMobarConf
    | otherwise       = spawnPipe ""

-- Other processes that need to run, depending on the bar
myBarAutostart :: String
myBarAutostart
    | myBar == XMobar   = "stalonetray --config " ++ myStalonetrayConf
    | myBar == Tint2    = "tint2 -c "             ++ myTint2Conf
    | myBar == Taffybar = "taffybar"

-- Symbols for displaying workspaces in xmobar
-- Must be functions, as it expects a different symbol for each
myCurrentWsSymbol workspaceName = "[●]" -- The workspace currently active
myHiddenWsSymbol  workspaceName =  "●"  -- Workspaces with open windows
myEmptyWsSymbol   workspaceName =  "○"  -- Workspaces with no windows

-- Data to be sent to the bar 
-- barProc points to the status bar's process handle
myXMobarLogHook :: Handle -> X ()
-- dynamicLogWithPP allows us to format the output
-- xmobarPP gives us some defaults
myXMobarLogHook barProc = dynamicLogWithPP xmobarPP
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

myLogHook :: Handle -> X ()
myLogHook barProc
    | myBar == XMobar = myXMobarLogHook barProc
    | otherwise       = def  -- Outputting an unread log can crash XMonad
