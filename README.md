## Setup on fresh install

1.  Dependencies:

    Required:
    ```bash
    git
    nerd-fonts-complete  # aur, for URxvt
    ```

    Optional:
    ```bash
    nvim
    neofetch
    exa
    ```

2.  Clone

    ```bash
    alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles-git/ --work-tree=$HOME'
    echo .dotfiles-git >> .gitignore
    git clone --bare https://github.com/quarkQuark/dotfiles.git $HOME/.dotfiles-git
    dotfiles checkout
    dotfiles config --local status.showUntrackedFiles no
    ```
    
3.  [Vim-plug](https://github.com/junegunn/vim-plug) must be installed for the vim configuration to work properly
    Vim also needs:
    ```bash
    nnn
    fzy
    ripgrep
    ```

4.  Xmonad is installed via `stack-static` from the aur
    ```bash
    cd ~/.config/xmonad
    stack install
    ```
    This also installs xmonad-contrib and xmobar
    
    My xmonad configuration also depends on the following packages:
    ```bash
    nitrogen
    aur/picom-ibhagwan-git
    xcape
    alacritty
    nvim
    networkmanager
    network-manager-applet
    udiskie
    volumeicon
    xfce4-power-manager
    rofi
    stalonetray
    qutebrowser
    spectacle
    zenity
    ```

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
