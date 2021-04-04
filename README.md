# My Configuration

![XMonad](/.config/Screenshots/2020-08-23-XMonad.png?raw=true)

Hello there!
These are my dotfiles, which are text-based configuration files for most of the programs
I use on Linux. I have dabbled in a few different window managers, but my current daily
driver - and hence the only one that is in any sort of working order - is XMonad.
See
[my XMonad config README.md](.config/xmonad/README.md)
for more XMonad-specific information!

## Setup on fresh install

1.  Dependencies:

    * Required: `aur/nerd-fonts-ubuntu-mono`
    * Optional: `exa neofetch nvim`
    I also use `autorandr` to automatically manage my monitors.

2.  Clone

    ```bash
    alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles-git/ --work-tree=$HOME'
    echo .dotfiles-git >> .gitignore
    git clone --bare https://github.com/quarkQuark/dotfiles.git $HOME/.dotfiles-git
    dotfiles checkout
    dotfiles config --local status.showUntrackedFiles no
    ```

3.  [Vim-plug](https://github.com/junegunn/vim-plug) must be installed
    for the vim configuration to work properly
    Vim also needs:
    ```bash
    nnn
    fzy
    ripgrep
    ```

4.  Xmonad is installed via `stack-static` from the aur:
    ```bash
    cd ~/.config/xmonad
    stack install
    cd ~/.config/taffybar
    stack install
    stack install xmobar
    stack install status-notifier-item  # For taffybar's system tray
    ```

    My xmonad configuration also depends on the following packages:
    * System:           `network-manager nitrogen aur/picom-ibhagwan-git`
    * Default programs: `alacritty nvim qutebrowser redshift rofi spectacle`
    * Bar applets:      `network-manager-applet udiskie pasystray`
    * For xmobar:       `stalonetray xfce4-power-manager`
    * For taffybar:     `lxqt-power-management`
    * Other:
      ``` bash
      dzen2  # For the keybinding cheatsheet
      xcape  # For rebinding C-Esc to super
      ```

5. Spotify can be themed and extended using `spicetify-cli`.
   My [spicetify config](.config/spicetify/config.ini) also requires some files from 
   https://github.com/3raxton/spicetify-custom-apps-and-extensions .

## Usage

*   Push local changes:
    ```bash
    dotfiles add <filepath>
    dotfiles commit -m "<message>"
    dotfiles push
    ```
*   Pull upstream changes:
    ```bash
    dotfiles pull
    ```

## How this repository was set up

1.  Initialise local bare git repository `.dotfiles-git`

    ```bash
    mkdir $HOME/.dotfiles-git
    git init --bare $HOME/.dotfiles-git
    alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles-git/ --work-tree=$HOME'
    dotfiles config --local status.showUntrackedFiles no
    ```

2.  Add some config files to back up (`.bashrc` used as example)

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
