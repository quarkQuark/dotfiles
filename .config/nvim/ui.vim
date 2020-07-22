set cursorline

" Needed for vim-hexokinase colour previews, but messes up urxvt
if has('termguicolors')
    set termguicolors
endif

" Use colour defined by my colourscheme script, if available
let colours = expand('$HOME/.config/nvim/colours.vim')
if filereadable(colours)
    exec 'source' colours
else
    " Default colourscheme
    colorscheme nord
endif

" Fix unreadable conceal colours in some colourschemes
hi Conceal guibg=Normal guifg=Normal ctermbg=None ctermfg=None

" Fix transparency in alacritty
hi! Normal  ctermbg=NONE guibg=NONE
hi! NonText ctermbg=NONE guibg=NONE guifg=NONE ctermfg=NONE

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" => Statusline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Hide default mode text, as airline has it instead
set noshowmode

" Allow airline to show the arrowy decorations
let g:airline_powerline_fonts=1

let g:airline_theme = 'powerlineish'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" => startify welcome screen
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:startify_padding_left = 22

let g:startify_lists = [
      \ { 'type': 'sessions',  'header': startify#pad(['Sessions'])     },
      \ { 'type': 'bookmarks', 'header': startify#pad(['Bookmarks'])    },
      \ { 'type': 'commands',  'header': startify#pad(['Commands'])     },
      \ { 'type': 'files',     'header': startify#pad(['Recent Files']) },
      \ ]

let g:startify_bookmarks = [
      \ { 'c': '~/Chords/index.txt' },
      \ ]

" Files to ignore in 'Recent Files'
" NB: Wildcard * must be written as .*
let g:startify_skiplist = [
      \ "Chords",
      \ ".local/share/nvim/plugged/.*/doc",
      \ "/usr/share/nvim/runtime/doc",
      \ ]

" Default is 'startify#pad(startify#fortune#cowsay())'
let g:startify_custom_header = [
      \ '       s++       so                                                                ',
      \ '     sss+++      ooso                                           NN                 ',
      \ '   yyyss++++     oooos                                          ""                 ',
      \ '   yyyys++++++   ooooo    NNNNNN   mNNNNNm   NNNNNN  Nmm    Nmm Nm  Nmm mmmNm mmmm ',
      \ '   yyyyy ++++++  sssss    N     m N       N mm    mm  mmm  Nmm  Nm  NmmN  mmmN  mmm',
      \ '   yyyyy  ++++++ sssss    N     m NNNNNNNN  N      N   mmNNmm   Nm  Nmm   Nmm   Nmm',
      \ '   yyyyy   ++++++sssss    N     m N         mm    mm    mmmm    Nm  Nmm   Nmm   Nmm',
      \ '   yyyyy    +++++yssss    N     m  NNNNNNY   NNNNNN      mm     Nm  Mmm   Nmm   Nmm',
      \ '   hyyyy     ++++yysss                                                             ',
      \ '     hhh      +++yyy                                                               ',
      \ '       d        +yF                                                                ',
      \ ]

"" May be useful in future
" Autoloads session if Session.vim found in current directory
"let g:startify_session_autoload = 1
