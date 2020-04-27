set cursorline
set termguicolors  " Necessary for vim-hexokinase colour previewing
hi CursorLine cterm=NONE ctermbg=8 ctermfg=NONE

colorscheme nord

"""""""""""""""""
"" => Statusline
"""""""""""""""""

" Hide default mode text, as airline has it instead
set noshowmode

" Allow airline to show the arrowy decorations
let g:airline_powerline_fonts=1

let g:airline_theme = 'powerlineish'
