import os
import socket

from libqtile import widget
from libqtile.widget import Spacer

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

def init_widget_defaults():
    widget_defaults = dict(
                           font='Ubuntu Mono',
                           fontsize=16,
                           padding=5,
                           background=bar_colours[2]
                           )
    return widget_defaults

def init_widget_list():
    widget_list = [
                   widget.Sep(
                       linewidth = 0,
                       padding = 6,
                       foreground = bar_colours[2],
                       background = bar_colours[0]
                       ),
                   widget.GroupBox(
                       fontsize = 16,
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
                   widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       foreground = bar_colours[2],
                       background = bar_colours[0]
                       ),
                   widget.WindowName(
                       font = "Ubuntu",
                       fontsize = 16,
                       foreground = bar_colours[5],
                       background = bar_colours[0],
                       padding = 10,
                       ),
                   widget.Clock(
                       font = "Ubuntu",
                       foreground = bar_colours[2],
                       background = bar_colours[9],
                       format="%a %d/%m - %l:%M:%S"
                       ),
                   widget.CurrentLayoutIcon(
                       scale = 0.6,
                       foreground = bar_colours[0],
                       background = bar_colours[3],
                       ),
                   widget.CurrentLayout(
                       font = 'Ubuntu',
                       foreground = bar_colours[0],
                       background = bar_colours[3],
                       padding = 10,
                       ),
                   widget.Systray(
                       background = bar_colours[9],
                       padding = 10,
                       icon_size = 25,
                       ),
                   widget.Sep(
                       linewidth = 0,
                       padding = 5,
                       foreground = bar_colours[0],
                       background = bar_colours[9]
                       ),
                   ]
    return widget_list
