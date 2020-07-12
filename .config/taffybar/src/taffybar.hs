{-# LANGUAGE OverloadedStrings #-}
import System.Taffybar
import System.Taffybar.Information.CPU
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.PollingGraph

cpuCallback = do
    (_, systemLoad, totalLoad) <- cpuLoad
    return [ totalLoad, systemLoad ]

main = do
    let
        clock = textClockNewWith defaultClockConfig
        workspaces = workspacesNew defaultWorkspacesConfig

        simpleConfig = defaultSimpleTaffyConfig
                        { startWidgets = [ workspaces ]
                        , endWidgets = [ sniTrayNew  -- Requires status-notifier-watcher
                                       , clock
                                       ]
                        }

    simpleTaffybar simpleConfig
