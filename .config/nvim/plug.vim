"""""""""""""""""""""""""""""""""""""""""
"" => Initialise Plugins
"""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.local/share/nvim/plugged')

"" Utility
Plug 'scrooloose/nerdtree', {'on':'NERDTreeToggle'}

"" General Coding
Plug 'w0rp/ale'  " linting

"" Python
Plug 'vim-scripts/indentpython.vim', {'for':'python'}
Plug 'nvie/vim-flake8', {'for':'python'}

"" Haskell

"" Aesthetics
Plug 'ryanoasis/vim-devicons'			" Adds icons
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'	" Adds icons to NERDTree; requires vim-devicons
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

"" Colourschemes
Plug 'arcticecestudio/nord-vim'

call plug#end()

""""""""""""""""""""""""""""""""""""""""

"let g:airline#extensions#ale#enabled = 1  " Integrate airline with ale for linting

""""""""""""""""""""""""""""""""""""""""
"" => NERDTree
""""""""""""""""""""""""""""""""""""""""

" Start automatically when vim is started with no arguments
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree

let NERDTreeQuitOnOpen = 1
let NERDTreeDirArrows = 1
let NERDTreeShowHidden = 1      " Toggle with I (Shift-i)

" Keybinding
nnoremap <Leader>f :NERDTreeToggle<CR>
