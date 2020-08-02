"""""""""""""""""""""""""""""""""""""""""
"" => Initialise Plugins
"""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.local/share/nvim/plugged')

"" Utility
Plug 'scrooloose/nerdtree', {'on':'NERDTreeToggle'}
Plug 'mcchrish/nnn.vim'
Plug 'srstevenson/vim-picker'  " fzy integration
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-repeat'        " make more plugins work with '.'

"" Text editing
Plug 'sirver/ultisnips'
Plug 'tpope/vim-surround'

"" General coding
Plug 'w0rp/ale'  " linting

"" Python
Plug 'vim-scripts/indentpython.vim', {'for':'python'}
Plug 'nvie/vim-flake8', {'for':'python'}

"" Haskell

"" Markdown
Plug 'plasticboy/vim-markdown', {'for':'markdown'}

"" LaTeX
Plug 'lervag/vimtex', {'for':'tex'}
Plug 'KeitaNakamura/tex-conceal.vim', {'for':'tex'}

"" Aesthetics
Plug 'mhinz/vim-startify', {'branch':'center'}
Plug 'ryanoasis/vim-devicons'			        " Adds icons
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'	" Adds icons to NERDTree; requires vim-devicons
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'rrethy/vim-hexokinase', {'do':'make hexokinase'}  " Preview colours in vim

"" Colourschemes
Plug 'arcticicestudio/nord-vim', {'branch':'develop'}
Plug 'icymind/neosolarized'
Plug 'dracula/vim', {'as':'dracula'}

call plug#end()

""""""""""""""""""""""""""""""""""""""""

" Integrate airline with ale for linting
let g:airline#extensions#ale#enabled = 1

""""""""""""""""""""""""""""""""""""""""
"" => NERDTree
""""""""""""""""""""""""""""""""""""""""

" Start automatically when vim is started with no arguments
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree

let NERDTreeQuitOnOpen = 1
let NERDTreeDirArrows = 1
let NERDTreeShowHidden = 1      " Toggle with I (Shift-i)

""""""""""""""""""""""""""""""""""""""""
"" => nnn.vim
""""""""""""""""""""""""""""""""""""""""

" Disable default mappings
let g:nnn#set_default_mappings = 0
" Show as a floating window
let g:nnn#layout = { 'window':{ 'width':0.9, 'height':0.6, 'highlight':'Debug' } }
" Start in nav-to-type mode
"let g:nnn#command = 'nnn -n'

""""""""""""""""""""""""""""""""""""""""
"" => Fuzzy finding
""""""""""""""""""""""""""""""""""""""""

" Use ripgrep to find files instead of fd or find
let g:picker_custom_find_executable = 'rg'
let g:picker_custom_find_flags = '--color never --files --hidden'

" Open file from current directory
nnoremap <leader>fy :PickerEdit %:p:h<CR>
nnoremap <leader>fh :PickerSplit %:p:h<CR>
nnoremap <leader>fv :PickerVsplit %:p:h<CR>

""""""""""""""""""""""""""""""""""""""""
"" => UltiSnips
""""""""""""""""""""""""""""""""""""""""

let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'

""""""""""""""""""""""""""""""""""""""""
"" => vim-hexokinase
""""""""""""""""""""""""""""""""""""""""

let g:Hexokinase_highlighters = ['backgroundfull']
let g:Hexokinase_refreshEvents = ['BufRead', 'BufWrite', 'TextChangedI', 'InsertLeave']
" Don't highlight english names of colours ('color_names')
let g:Hexokinase_optInPatterns = ['full_hex', 'triple_hex', 'rgb', 'rgba', 'hsl', 'hsla']
