"""""""""""""""""""""""""""""""""""""""""
"" => Initialise Plugins
"""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.local/share/nvim/plugged')

"" Utility
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }

"" General Coding
Plug 'vim-syntastic/syntastic'

"" Python
Plug 'vim-scripts/indentpython.vim', { 'for': 'python' }
Plug 'nvie/vim-flake8', { 'for': 'python' }

"" Aesthetics
Plug 'ryanoasis/vim-devicons'			" Adds icons
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'	" Adds icons to NERDTree; requires vim-devicons
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

"""""""""""""""""""""""""""""""""""""""""
"" => Modules
"""""""""""""""""""""""""""""""""""""""""

" Set spacebar as leader key (must be done first)
let mapleader="\<SPACE>"
nnoremap <SPACE> <Nop>

" General settings
source $HOME/.config/nvim/conf.vim

" Plugin configuration
source $HOME/.config/nvim/plug.vim

" Keybindings
source $HOME/.config/nvim/keys.vim

" Aesthetics and Statusline (Powerline)
source $HOME/.config/nvim/ui.vim
