# DEBUGGING: $ qtile -l ERROR (lowercase L)
#
# The following comments are the copyright and licensing information from the default
# qtile config. Copyright (c) 2010 Aldo Cortesi, 2010, 2014 dequis, 2012 Randall Ma,
# 2012-2014 Tycho Andersen, 2012 Craig Barnes, 2013 horsik, 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this
# software and associated documentation files (the "Software"), to deal in the Software
# without restriction, including without limitation the rights to use, copy, modify,
# merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to the following
# conditions:
#
# The above copyright notice and this permission notice shall be included in all copies
# or substantial portions of the Software.

########################################
## => IMPORTS
########################################

import os
import subprocess
import socket
from libqtile.config import Key, Screen, Group, Drag, Click
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook

from typing import List  # noqa: F401

## My modules
from keys import init_keys

########################################

mod = "mod4"
myTerm = "urxvtc"

keys = init_keys(mod,myTerm)

groups = [Group(i) for i in "12345678"]

for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod], i.name, lazy.group[i.name].toscreen()),

        # mod1 + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"], i.name, lazy.window.togroup(i.name)),
    ])

########################################
## => LAYOUTS
########################################

layout_theme = {
                "border_width": 2,
                "margin": 10,
                "border_focus": "AD69AF",
                "border_normal": "1D2330"
                }
layouts = [
           layout.Max(**layout_theme),
           layout.MonadTall(**layout_theme),
           layout.MonadWide(**layout_theme),
           layout.Bsp(**layout_theme),
           layout.Columns(**layout_theme),
           layout.RatioTile(**layout_theme),
           layout.VerticalTile(**layout_theme),
           layout.Tile(shift_windows=True, **layout_theme),
           layout.Matrix(**layout_theme),
           layout.Zoomy(**layout_theme),
           layout.Floating(**layout_theme)
           ]

########################################
## => PANEL
########################################

bar_colours = [["#292D3E", "#292D3E"], # panel background
               ["#434758", "#434758"], # background for current screen tab
               ["#D0D0D0", "#D0D0D0"], # font color for group names
               ["#F07178", "#F07178"], # background color for layout widget
               ["#000000", "#000000"], # background for other screen tabs
               ["#AD69AF", "#AD69AF"], # dark green gradiant for other screen tabs
               ["#C3E88D", "#C3E88D"], # background color for network widget
               ["#C792EA", "#C792EA"], # background color for pacman widget
               ["#9CC4FF", "#9CC4FF"], # background color for cmus widget
               ["#000000", "#000000"], # background color for clock widget
               ["#434758", "#434758"]] # background color for systray widget

widget_defaults = dict(
                    font='Ubuntu Mono',
                    fontsize=12,
                    padding=2,
                    background=bar_colours[2]
                    )
extension_defaults = widget_defaults.copy()

prompt = "{0}@{1}: ".format(os.environ["USER"], socket.gethostname())
widget_list = [
                widget.Sep(
                    linewidth = 0,
                    padding = 6,
                    foreground = bar_colours[2],
                    background = bar_colours[0]
                    ),
                widget.GroupBox(
                    fontsize = 12,
                    margin_y = 0,
                    margin_x = 0,
                    padding_y = 5,
                    padding_x = 5,
                    borderwidth = 1,
                    active = bar_colours[2],
                    inactive = bar_colours[2],
                    rounded = False,
                    highlight_method = "block",
                    this_current_screen_border = bar_colours[1],
                    this_screen_border = bar_colours [4],
                    other_current_screen_border = bar_colours[0],
                    other_screen_border = bar_colours[0],
                    foreground = bar_colours[2],
                    background = bar_colours[0],
                    hide_unused = True
                    ),
                widget.Prompt(
                    prompt=prompt,
                    font="Ubuntu Mono",
                    padding=10,
                    foreground = bar_colours[3],
                    background = bar_colours[1]
                    ),
                widget.Sep(
                    linewidth = 0,
                    padding = 10,
                    foreground = bar_colours[2],
                    background = bar_colours[0]
                    ),
                widget.WindowName(font="Ubuntu",
                    fontsize = 12,
                    foreground = bar_colours[5],
                    background = bar_colours[0],
                    padding = 5
                    ), 
                widget.Image(
                    scale = True,
                    filename = "~/.config/qtile/bar06.png",
                    background = bar_colours[6]
                    ),
                widget.Image(
                    scale = True,
                    filename = "~/.config/qtile/bar02-b.png",
                    backgrund = bar_colours[6]
                    ),
                widget.Image(
                    scale = True,
                    filename = "~/.config/qtile/bar03.png",
                    background = bar_colours[3]
                    ),
                widget.CurrentLayoutIcon(
                    scale = 0.6,
                    foreground=bar_colours[0],
                    background=bar_colours[3],
                    ),
                widget.CurrentLayout(
                    foreground = bar_colours[0],
                    background = bar_colours[3],
                    padding = 5
                    ),
                widget.Image(
                    scale = True,
                    filename = "~/.config/qtile/bar04.png",
                    background = bar_colours[7]
                    ),
                widget.TextBox(
                    font="Ubuntu Bold",
                    text=" ⟳",
                    padding = 5,
                    foreground=bar_colours[0],
                    background=bar_colours[7],
                    fontsize=14
                    ),
                widget.Pacman(
                    execute = "urxvtc",
                    update_interval = 1800,
                    foreground = bar_colours[0],
                    background = bar_colours[7]
                    ),
                widget.TextBox(
                    text="Updates",
                    padding = 5,
                    foreground=bar_colours[0],
                    background=bar_colours[7]
                    ),
                widget.Image(
                    scale = True,
                    filename = "~/.config/qtile/bar05.png",
                    background = bar_colours[8]
                    ),
                widget.Image(
                    scale = True,
                    filename = "~/.config/qtile/bar07.png",
                    background = bar_colours[9]
                    ),
                widget.Systray(
                    background=bar_colours[9],
                    padding = 5
                    ),
                widget.Sep(
                    linewidth = 0,
                    padding = 6,
                    foreground = bar_colours[0],
                    background = bar_colours[9]
                    ),
                widget.TextBox(
                    font="Ubuntu Bold",
                    text=" 🕒",
                    foreground=bar_colours[2],
                    background=bar_colours[9],
                    padding = 5,
                    fontsize=14
                    ),
                widget.Clock(
                    foreground = bar_colours[2],
                    background = bar_colours[9],
                    format="%A, %d %B - %H:%M"
                    ),
                widget.Sep(
                    linewidth = 0,
                    padding = 5,
                    foreground = bar_colours[0],
                    background = bar_colours[9]
                    ),
                ]

screens = [
           Screen(top=bar.Bar(widgets=widget_list, opacity=0.95, size=20))
           ]

########################################

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
focus_on_window_activation = "smart"

## Startup applications
@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/autostart.sh'])

## Needed for some java apps
#wmname = "LG3D"
wmname = "qtile"
