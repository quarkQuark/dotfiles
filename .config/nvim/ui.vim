set cursorline

colorscheme nord
set background=dark

" Fixes transparency in alacritty
hi! Normal  ctermbg=NONE guibg=NONE
hi! NonText ctermbg=NONE guibg=NONE guifg=NONE ctermfg=NONE

" Needed for vim-hexokinase colour previews, but messes up urxvt
if has('termguicolors')
    set termguicolors
endif

"""""""""""""""""
"" => Statusline
"""""""""""""""""

" Hide default mode text, as airline has it instead
set noshowmode

" Allow airline to show the arrowy decorations
let g:airline_powerline_fonts=1

let g:airline_theme = 'powerlineish'
