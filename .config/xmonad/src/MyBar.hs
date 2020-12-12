module MyBar
where

import System.IO
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import MyPrograms

spawnBarProcCmd :: Bar -> String
spawnBarProcCmd XMobar = "xmobar " ++ myConfigDir ++ "xmobarrc.hs"
spawnBarProcCmd other  = ""

mySpawnBar :: IO (Handle)
mySpawnBar = spawnPipe (spawnBarProcCmd myBar)

-- Processes that need starting
myBarAutostart :: Bar -> String
myBarAutostart XMobar   = "stalonetray --config " ++ myConfigDir ++ "stalonetrayrc"
myBarAutostart Tint2    = "tint2 -c ~/.config/tint2/xmonad.tint2rc"
myBarAutostart Taffybar = "status-notifier-watcher && taffybar" -- From status-notifier-item

-- Symbols for displaying workspaces in xmobar
-- Must be functions, as it expects a different symbol for each
myCurrentWsSymbol workspaceName = "[●]" -- The workspace currently active
myHiddenWsSymbol  workspaceName =  "●"  -- Workspaces with open windows
myEmptyWsSymbol   workspaceName =  "○"  -- Workspaces with no windows

-- Data to be sent to the bar 
-- barProc points to the status bar's process handle
-- dynamicLogWithPP allows us to format the output
-- xmobarPP gives us some defaults
whichLogHook XMobar barProc = dynamicLogWithPP xmobarPP
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
-- Dont't output a log for other bars (can crash XMonad)
whichLogHook other barProc = def
myLogHook = whichLogHook myBar
