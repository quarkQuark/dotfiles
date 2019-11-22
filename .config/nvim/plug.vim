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

""""""""""""""""""""""""""""""""""""""""
"" => Python
""""""""""""""""""""""""""""""""""""""""

au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix
    \ match BadWhitespace= /\s\+$/

let python_highlight_all=1
syntax on
