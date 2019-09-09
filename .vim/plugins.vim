filetype off  " required

" Set the runtime path to include Vundle and initialise
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"" Utility
Plugin 'scrooloose/nerdtree'
Plugin 'severin-lemaignan/vim-minimap'
"Plugin 'scrooloose/nerdcommenter'
"Plugin 'majutsushi/tagbar'
"Plugin 'jceb/vim-orgmode'
"Plugin 'christoomey/vim-tmux-navigator'
"Plugin 'jreybert/vimagit'

"" Programming
Plugin 'neovimhaskell/haskell-vim'
"Plugin 'universal-ctags/ctags'
"Plugin 'konfekt/fastfold'

"" Python
"Plugin 'w0rp/ale'
"Plugin 'python-mode/python-mode'
"Plugin 'tmhedberg/simpylfold'

"" Aesthetics
Plugin 'ryanoasis/vim-devicons'                     " Adds icons to vim
Plugin 'tiagofumo/vim-nerdtree-syntax-highlight'    " Adds icons to nerdtree; requires above


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
call vundle#end()
