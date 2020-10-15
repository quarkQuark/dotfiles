" Case-sensitivity for searching
set ignorecase      " Searching is case insensitive
set smartcase       " ... unless any capital letters are used

" Tab behaviour
set expandtab       " Insert spaces when TAB is pressed
set tabstop=4       " Render TABs using this many spaces
set softtabstop=4   "
set shiftwidth=4    " Indentation amount for > and < commands


" Scroll when the cursor is within 3 lines of bottom
set scrolloff=3

" Mouse compatibility for all modes
set mouse=a

" Use system clipboard
set clipboard+=unnamedplus

" Recognise all *.tex files as LaTeX
"let g:tex_flavor = 'latex' " Moved to ftplugin
" Recognise .cls files as LaTeX
autocmd BufRead,BufNewFile *.cls setlocal filetype=tex

" Other
set showmatch       " Show matching brackets
