# DEBUGGING: $ qtile -l ERROR (lowercase L)
#   Then restart with $ qtile-cmd -o cmd -f restart
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
from libqtile.widget import Spacer

from powerline.bindings.qtile.widget import PowerlineTextBox

from typing import List  # noqa: F401

## My modules
from keys import init_keys
from widgets import init_widget_defaults, init_widget_list
import powerline_widgets

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
                "border_width": 3,
                "margin": 3,
                "border_focus": "5E0000",
                "border_normal": "1D2330"
                }
layouts = [
           layout.MonadTall(**layout_theme),
           layout.MonadWide(**layout_theme),
           layout.Max(**layout_theme)
           ]

########################################
## => BAR
########################################

#widget_defaults = init_widget_defaults()
#extension_defaults = widget_defaults.copy()
#
#widget_list = init_widget_list()
#
#screens = [
#           Screen(top=bar.Bar(widgets=widget_list, opacity=0.95, size=25))
#           ]

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

## Polybar
@hook.subscribe.startup  # When qtile starts or restarts
def init_polybar():
    polybar_launch = os.path.expanduser('~/.config/polybar/launch.sh')
    subprocess.call([polybar_launch])

## Startup applications
@hook.subscribe.startup_once  # Only when qtile starts
def start_once():
    autostart = os.path.expanduser('~/.config/qtile/autostart.sh')
    subprocess.call([autostart])

## Needed for some java apps
#wmname = "LG3D"
wmname = "qtile"
