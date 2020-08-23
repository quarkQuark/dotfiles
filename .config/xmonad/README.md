# My XMonad Configuration

![](../Screenshots/2020-08-23-XMonad.png?raw=true)

XMonad is my favourite window manager, configured in Haskell, my favourite programming language.
I've configured it quite heavily already, and envisage much more configuration in the
future! Unlike most tiling window manager fans, I'm not afraid of a little bloat or eyecandy!

## Installation

The installation instructions for this config currently live at
[the root README file](/../../README.md)

## Panels

I've configured XMonad to work with three different panels.
The current panel can be easily changed by changing the `myBar` variable in
[xmonad.hs](/src/xmonad.hs "XMonad with Tint2)

### Tint2

The most bloated. The main reason I like tint2 is that I can have a vertical panel on the
left of my screen, a feature that is inexplicably left out of pretty much every other
desktop-agnostic panel. Tint2 is also the easiest to configure as it has an optional graphical
configuration manager.

This is the panel shown in the above screenshot.

### XMobar

The standard XMonad status bar. Very simple and lightweight, and extremely well integrated
with XMonad itself. However, it sometimes seems a little *too* lightweight.

### Taffybar

Truly the XMonad of status bars, in that it is really a library to build your own.
A very promising project, but unfortunately my Haskell-fu isn't yet strong enough
to do anything impressive with it. And I'm pretty sure it doesn't do vertical bars.

## Keybindings

One of the most unique features of this config is the keybinding cheatsheet that can be
brought up at any time with `Super+?`, inspired by AwesomeWM.
This is achieved with `dzen2` and
[a shell script with lots of regex](/.scripts/dzen2-display-cheatsheet).
In [src/modifiers.sh](./src/modifiers.sh) I also bind `Caps Lock` to `Hyper`
and `Space`, when held down, to an additional `Super` (Windows) key.
The goal of this is to enable more ergonomic keybindings.

Eventually I will include a list of keybindings in this README,
but until then refer to the screenshot at the top of this page.
