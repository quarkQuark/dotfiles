from libqtile.config import Key
from libqtile.command import lazy

@lazy.function
def window_to_prev_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i - 1].name)

@lazy.function
def window_to_next_group(qtile):
    if qtile.currentWindow is not None:
        i = qtile.groups.index(qtile.currentGroup)
        qtile.currentWindow.togroup(qtile.groups[i + 1].name)

def init_keys(mod,myTerm):
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
                [mod], "q",
                lazy.restart()                      # Restart Qtile
                ),
            Key(
                [mod, "shift"], "q",
                lazy.shutdown()                     # Shutdown Qtile
                ),
            Key(
                [mod, "control"], "j",
                lazy.layout.section_up()            # Move up a section in treetab
                ),
            Key(
                [mod, "control"], "k",
                lazy.layout.section_down()          # Move down a section in treetab
                ),
            ### Window controls
            Key(
                [mod], "j",
                lazy.layout.down()                  # Switch between windows in current stack pane
                ),
            Key(
                [mod], "k",
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
                lazy.layout.increase_nmaster(),     # Increase number in master pane (Tile)
                ),
            Key(
                [mod, "shift"], "h",
                lazy.layout.shrink(),               # Shrink size of current window (XmonadTall)
                lazy.layout.decrease_nmaster(),     # Decrease number in master pane (Tile)
                ),
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
                [mod], "f",
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
            ### Dmenu Run Launcher  (C-esc is mapped to Windows key on its own)
            Key(
                ["control"], "Escape",
                lazy.spawn("dmenu_run -fn \
                                'UbuntuMono Nerd Font:size=12' \
                                -nb '#292d3e' \
                                -nf '#bbc5ff' \
                                -sb '#82AAFF' \
                                -sf '#292d3e' \
                                -p 'dmenu:'\
                            ")
                ),
            ### Dmenu scripts launched with MOD + ALT + KEY
            Key(
                [mod, "mod1"], "e",
                lazy.spawn("./.dmenu/dmenu-edit-configs.sh")
                ),
            Key(
                [mod, "mod1"], "m",
                lazy.spawn("./.dmenu/dmenu-sysmon.sh")
                ),
            Key(
                [mod, "mod1"], "p",
                lazy.spawn("passmenu")
                ),
            Key(
                [mod, "mod1"], "r",
                lazy.spawn("./.dmenu/dmenu-reddio.sh")
                ),
            Key(
                [mod, "mod1"], "s",
                lazy.spawn("./.dmenu/dmenu-surfraw.sh")
                ),
            Key(
                [mod, "mod1"], "t",
                lazy.spawn("./.dmenu/dmenu-trading.sh")
                ),
            ### Applications
            #Key(
                #[mod], "w",
                #lazy.spawn(myTerm+" -e surf")
                #),
            Key(
                [mod, "shift"], "w",
                lazy.spawn("firefox")
                ),
            Key(
                [mod, "shift"], "e",
                lazy.spawn("emacsclient")
                ),
            Key(
                [mod], "f",
                lazy.spawn(myTerm+" -e vifm")
                ),
            #Key(
                #[mod, "mod1"], "j",
                #lazy.spawn(myTerm+" -e joplin")
                #),
            #Key(
                #[mod, "mod1"], "y",
                #lazy.spawn(myTerm+" -e youtube-viewer")
                #),
            #Key(
                #[mod, "mod1"], "a",
                #lazy.spawn(myTerm+" -e ncpamixer")
                #),
    ]
    return keys
