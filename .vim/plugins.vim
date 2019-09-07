filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
""""""""""""""""""""

""""""""""""""""""""
"" Plugins
""""""""""""""""""""

" Utility
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'majutsushi/tagbar'
Plugin 'jceb/vim-orgmode'
Plugin 'christoomey/vim-tmux-navigator'

" Programming
Plugin 'universal-ctags/ctags'
Plugin 'altercation/vim-colors-solarized'
Plugin 'konfekt/fastfold'

" Python
Plugin 'w0rp/ale'
Plugin 'python-mode/python-mode'
Plugin 'tmhedberg/simpylfold'

""""""""""""""""""""
call vundle#end()
