module MyBar
(barSpawnPipe', barAutostart', barLogHook')
where

import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import Options

data BarCommand = BarCommand
    { barSpawnPipe :: IO (Handle)    -- Command to start bar with handle
    , barAutostart :: X ()           -- Autostart programs dependent on bar
    , barLogHook   :: Handle -> X () -- Data XMonad needs to send to the bar
    }

defBarCommand = BarCommand
    { barSpawnPipe = spawnPipe ""
    , barAutostart = spawnOnce ""
    , barLogHook   = def
    }

barCommand :: Bar -> BarCommand

barCommand XMobar = defBarCommand
    { barSpawnPipe = spawnPipe $ "xmobar " ++ myXMobarConf
    , barAutostart = spawnOnce $ "stalonetray --config " ++ myStalonetrayConf
      -- dynamicLogWithPP allows us to format the output
      -- xmobarPP gives us some defaults
    , barLogHook   = \h -> dynamicLogWithPP xmobarPP
          -- Write to bar instead of stdout
          { ppOutput          = hPutStrLn h
          -- How to order the different sections of the log
          , ppOrder           = \(workspace:layout:title:extras)
                              -> [workspace,layout]
          -- Separator between different sections of the log
          , ppSep             = "  "
          -- Format the workspace information
          , ppCurrent         = wsSymb "[●]" -- The workspace currently active
          , ppHidden          = wsSymb "●"   -- Workspaces with open windows
          , ppHiddenNoWindows = wsSymb "○"   -- Workspaces with no windows
          }
    }
  where
    -- xmobarPP expects function of workspace name
    wsSymb s workspace = xmobarColor "white" "" s

barCommand Tint2 = defBarCommand
    { barAutostart = spawnOnce $ "tint2 -c " ++ myTint2Conf }

barCommand Taffybar = defBarCommand
    { barAutostart = spawnOnce "taffybar" }

barSpawnPipe' = barSpawnPipe . barCommand
barAutostart' = barAutostart . barCommand
barLogHook'   = barLogHook   . barCommand
