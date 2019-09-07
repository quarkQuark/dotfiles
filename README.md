## Setup on fresh install

1.  Install:
    ````bash
    git
    neofetch
    powerline
    powerline-fonts
    ````

2.  Clone

    ````bash
    alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles-git/ --work-tree=$HOME'
    echo .dotfiles-git >> .gitignore
    git clone --bare https://github.com/quarkQuark/dotfiles.git $HOME/.dotfiles-git
    dotfiles checkout
    dotfiles config --local status.showUntrackedFiles no
    ````
    
3.  Vundle must be installed for the vim configuration to work properly:
    ````bash
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
    vim +PluginInstall +qall
    ````

## Usage

*   Push local changes:
    ````bash
    dotfiles add <filepath>
    dotfiles commit -m "<message>"
    dotfiles push
    ````
*   Pull upstream changes:
    ````bash
    dotfiles pull
    ````

## How this repository was set up

1.  Initialise local bare git repository `.dotfiles-git`

    ````bash
    mkdir $HOME/.dotfiles-git
    git init --bare $HOME/.dotfiles-git
    alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles-git/ --work-tree=$HOME'
    dotfiles config --local status.showUntrackedFiles no
    ````

2.  Add some config files to back up (`.bashrc` used as example)

    ````bash
    git add .bashrc
    git commit -m "Add .bashrc"
    ````

3.  Create new GitHub repository `dotfiles`

4.  Link the two

    ````bash
    dotfiles remote add origin https://github.com/quarkQuark/dotfiles.git
    dotfiles push -u origin master
    ````
