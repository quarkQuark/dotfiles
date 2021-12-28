# My XMonad Configuration

![](../Screenshots/2020-08-23-XMonad.png?raw=true)

XMonad is a library for creating a tiling window manager in Haskell,
a programming language with particular appeal to me as a mathematician.
Proponents of tiling window managers often praise their minimalism.
While appreciating how fast they run, I have also put effort into keeping around some eyecandy!

## Installation

The installation instructions for this config currently live at
[the root README file](/../../README.md)
for this repository.

## Keybindings

One of the most unique features of this config is the keybinding cheatsheet that can be
brought up at any time with `Super+?`, inspired by AwesomeWM.
This is achieved with
[XMonad.Util.NamedActions](https://www.stackage.org/haddock/lts-18.5/xmonad-contrib-0.16/XMonad-Util-NamedActions.html)
and displayed using `dzen2` and
[a shell script with lots of regex](/.scripts/dzen2-display-cheatsheet).

In [src/modifiers.sh](./src/modifiers.sh) I also bind `Caps Lock` to `Hyper`
and `Space`, when held down, to an additional `Super` (Windows) key.
The goal of this is to enable more ergonomic keybindings.
Eventually I will include a list of keybindings in this README,
but until then refer to the screenshot at the top of this page.

## Panels

I've configured XMonad to work with three different panels.
The current panel can be easily changed by changing the `myBar` variable in [app/Main.hs](/app/Main.hs).

* Tint2

  The most bloated.
  The main reason I like Tint2 is that I can have a vertical panel on the left of my screen,
  a feature that is sorely missing from pretty much every other desktop-agnostic panel.
  Tint2 is also the easiest to configure as it has an optional graphical configuration manager.

  This is the panel shown in the above screenshot.

* XMobar

  The standard XMonad status bar.
  Very simple and lightweight, and extremely well integrated with XMonad itself.
  This is the least buggy, but also the least pretty.

* Taffybar

  Truly the XMonad of status bars, in that it is really a library to build your own.
  However, it has very little documentation.
