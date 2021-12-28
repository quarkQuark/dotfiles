# My Configuration

![XMonad](/.config/Screenshots/2020-08-23-XMonad.png?raw=true)

These are my dotfiles,
which are text-based configuration files for most of the programs I use on Linux.
The two most important programs I use are the window manager and text editor.

* The window manager controls the bulk of the desktop experience
  by deciding where applications appear on the screen.
  I prefer to use tiling window managers,
  as they allow me to control almost everything through keyboard shortcuts.

  I have dabbled in a few, but my current daily driver
  — and hence the only one whose configuration files are in any sort of working order
  — is **XMonad**.
  XMonad is really more of a library for configuring your own window manager
  using the Haskell programming language.
  Further information, and my XMonad configuration, can be found
  [here](.config/xmonad/README.md).

* I used to use **Neovim** as my text editor (config [here](.config/nvim)),
  which is based on Vim and treats keyboard shortcuts almost as their own language.
  More recently, I have switched to Emacs.
  **Emacs** is much, much more extensible than Vim, and readily emulates Vim keybindings.
  One particular benefit is the incredible **Org Mode** which, amongst many other things,
  allows me to automatically export a literate version of my config to
  [this webpage](https://quarkQuark.github.io/literate-config/emacs).

## Setup on fresh Arch Linux install

1.  Dependencies

    * Required: `nerd-fonts-ubuntu-mono` from the Arch User Repository (AUR)
    * Optional: `exa neofetch nvim`

2.  Clone this repository

    ```bash
    alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles-git/ --work-tree=$HOME'
    echo .dotfiles-git >> .gitignore
    git clone --bare https://github.com/quarkQuark/dotfiles.git $HOME/.dotfiles-git
    dotfiles checkout
    dotfiles config --local status.showUntrackedFiles no
    ```

3.  Follow the README instructions to install [Vim-plug](https://github.com/junegunn/vim-plug).
    My configuration also needs the packages `nnn`, `fzy` and `ripgrep`

4.  Xmonad is installed via `stack-static` from the AUR.
    Arch has packages available from the standard repositories,
	but these are prone to breakage due to dynamic linking.

    ```bash
    cd ~/.config/xmonad && stack install
    cd ~/.config/taffybar && stack install
    stack install xmobar
    stack install status-notifier-item  # For taffybar's system tray
    ```

5. My XMonad configuration also depends on the following packages:

    * System:           `network-manager nitrogen aur/picom-ibhagwan-git`
    * Default programs: `alacritty nvim qutebrowser redshift rofi spectacle`
    * Bar applets:      `network-manager-applet udiskie pasystray`
    * For xmobar:       `stalonetray xfce4-power-manager`
    * For taffybar:     `lxqt-power-management`
    * Other:

      ``` bash
      dzen2  # For the keybinding cheatsheet
      xcape  # For rebinding C-Esc to the Super for extra keyboard shortcuts
      ```

5. Spotify can be themed and extended using `spicetify-cli`.
   My [spicetify config](.config/spicetify/config.ini) also requires some files from 
   https://github.com/3raxton/spicetify-custom-apps-and-extensions .

## Contributing

*   Push local changes:

    ```bash
    dotfiles add <filepath>
    dotfiles commit -m "<message>"
    dotfiles push
    ```

*   Pull upstream changes

    ```bash
    dotfiles pull
    ```

## How I set up this repository

1.  Initialise `.dotfiles-git` as a local bare repository 

    ```bash
    mkdir $HOME/.dotfiles-git
    git init --bare $HOME/.dotfiles-git
    alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles-git/ --work-tree=$HOME'
    dotfiles config --local status.showUntrackedFiles no
    ```

2.  Add some config files to back up (`.bashrc` used as an example)

    ```bash
    git add .bashrc
    git commit -m "Add .bashrc"
    ```

3.  Create new GitHub repository `dotfiles`

4.  Link the two

    ```bash
    dotfiles remote add origin https://github.com/quarkQuark/dotfiles.git
    dotfiles push -u origin master
    ```
