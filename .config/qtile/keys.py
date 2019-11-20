def init_keys():
    keys = [
            Key(
                [mod], "Return",
                lazy.spawn(myTerm)                  # Open terminal
                ),
            Key(
                [mod], "Tab",
                lazy.next_layout()                  # Toggle trough layouts
                ),
            Key(
                [mod, "shift"], "c",
                lazy.window.kill()                  # Kill active window
                ),
            Key(
                [mod, "shift"], "r",
                lazy.restart()                      # Restart Qtile
                ),
            Key(
                [mod, "shift"], "q",
                lazy.shutdown()                     # Shutdown Qtile
                ),
            Key(
                [mod], "w",
                lazy.to_screen(2)                   # Keyboard focus screen(0)
                ),
            Key(
                [mod], "e",
                lazy.to_screen(0)                   # Keyboard focus screen(1)
                ),
            Key(
                [mod], "r",
                lazy.to_screen(1)                   # Keyboard focus screen(2)
                ),
            Key(
                [mod, "control"], "k",
                lazy.layout.section_up()            # Move up a section in treetab
                ),
            Key(
                [mod, "control"], "j",
                lazy.layout.section_down()          # Move down a section in treetab
                ),
            ### Window controls
            Key(
                [mod], "k",
                lazy.layout.down()                  # Switch between windows in current stack pane
                ),
            Key(
                [mod], "j",
                lazy.layout.up()                    # Switch between windows in current stack pane
                ),
            Key(
                [mod, "shift"], "k",
                lazy.layout.shuffle_down()          # Move windows down in current stack
                ),
            Key(
                [mod, "shift"], "j",
                lazy.layout.shuffle_up()            # Move windows up in current stack
                ),
            Key(
                [mod, "shift"], "l",
                lazy.layout.grow(),                 # Grow size of current window (XmonadTall)
                lazy.layout.increase_nmaster(),     # Increase number in master pane (Tile)                 ),
            Key(
                [mod, "shift"], "h",
                lazy.layout.shrink(),               # Shrink size of current window (XmonadTall)
                lazy.layout.decrease_nmaster(),     # Decrease number in master pane (Tile)                     ),
            Key(
                [mod, "shift"], "Left",             # Move window to workspace to the left
                window_to_prev_group
                ),
            Key(
                [mod, "shift"], "Right",            # Move window to workspace to the right
                window_to_next_group
                ),
            Key(
                [mod], "n",
                lazy.layout.normalize()             # Restore all windows to default size ratios
                ),
            Key(
                [mod], "m",
                lazy.layout.maximize()              # Toggle a window between minimum and maximum sizes
                ),
            Key(
                [mod, "shift"], "KP_Enter",
                lazy.window.toggle_floating()       # Toggle floating
                ),
            Key(
                [mod, "shift"], "space",
                lazy.layout.rotate(),               # Swap panes of split stack (Stack)
                lazy.layout.flip()                  # Switch which side main pane occupies (XmonadTall)
                ),
            ### Stack controls
            Key(
                [mod], "space",
                lazy.layout.next()                  # Switch window focus to other pane(s) of stack
                ),
            Key(
                [mod, "control"], "Return",
                lazy.layout.toggle_split()          # Toggle between split and unsplit sides of stack
                ),
            ### Dmenu Run Launcher
            Key(
                ["mod1", "control"], "Return",
                lazy.spawn("dmenu_run -fn \
                                'UbuntuMono Nerd Font:size=10' \
                                -nb '#292d3e' \
                                -nf '#bbc5ff' \
                                -sb '#82AAFF' \
                                -sf '#292d3e' \
                                -p 'dmenu:'\
                            ")
                ),
            ### Dmenu scripts launched with ALT + CTRL + KEY
            Key(
                ["mod1", "control"], "e",
                lazy.spawn("./.dmenu/dmenu-edit-configs.sh")
                ),
            Key(
                ["mod1", "control"], "m",
                lazy.spawn("./.dmenu/dmenu-sysmon.sh")
                ),
            Key(
                ["mod1", "control"], "p",
                lazy.spawn("passmenu")
                ),
            Key(
                ["mod1", "control"], "r",
                lazy.spawn("./.dmenu/dmenu-reddio.sh")
                ),
            Key(
                ["mod1", "control"], "s",
                lazy.spawn("./.dmenu/dmenu-surfraw.sh")
                ),
            Key(
                ["mod1", "control"], "t",
                lazy.spawn("./.dmenu/dmenu-trading.sh")
                ),
            ### Applications launched with SUPER + ALT + KEY
            Key(
                [mod, "mod1"], "l",
                lazy.spawn(myTerm+" -e lynx -cfg=~/.lynx/lynx.cfg -lss=~/.lynx/lynx.lss)
                ),
            Key(
                [mod, "mod1"], "n",
                lazy.spawn(myTerm+" -e newsboat")
                ),
            Key(
                [mod, "mod1"], "r",
                lazy.spawn(myTerm+" -e rtv")
                ),
            Key(
                [mod, "mod1"], "e",
                lazy.spawn(myTerm+" -e neomutt")
                ),
            Key(
                [mod, "mod1"], "m",
                lazy.spawn(myTerm+" -e sh ./scripts/toot.sh")
                ),
            Key(
                [mod, "mod1"], "t",
                lazy.spawn(myTerm+" -e sh ./scripts/tig-script.sh")
                ),
            Key(
                [mod, "mod1"], "f",
                lazy.spawn(myTerm+" -e sh ./.config/vifm/scripts/vifmrun")
                ),
            Key(
                [mod, "mod1"], "j",
                lazy.spawn(myTerm+" -e joplin")
                ),
            Key(
                [mod, "mod1"], "c",
                lazy.spawn(myTerm+" -e cmus")
                ),
            Key(
                [mod, "mod1"], "i",
                lazy.spawn(myTerm+" -e irssi")
                ),
            Key(
                [mod, "mod1"], "y",
                lazy.spawn(myTerm+" -e youtube-viewer")
                ),
            Key(
                [mod, "mod1"], "a",
                lazy.spawn(myTerm+" -e ncpamixer")
                ),
    ]
    return keys
