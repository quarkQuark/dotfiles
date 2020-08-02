" I reserve m for filetype keybindings

" Use jk as escape
inoremap jk <esc>

" Visual line movement
nnoremap j gj
nnoremap k gk

" Matches behaviour of C and D (by default, Y is yy)
nnoremap Y y$

" Clear search highlights
nnoremap <Leader>/ :nohlsearch<CR>

" Split navigation
set splitbelow
set splitright

nnoremap <Leader>w- :sp<CR>
nnoremap <Leader>w/ :vsp<CR>
nnoremap <Leader>w= <C-W>=
nnoremap <Leader>wh <C-W><C-H>
nnoremap <Leader>wj <C-W><C-J>
nnoremap <Leader>wk <C-W><C-K>
nnoremap <Leader>wl <C-W><C-L>
nnoremap <Leader>ww <C-W>w

"" File browsing
" previous file
nnoremap <Leader>fb :e#
" fzf
nnoremap <Leader>ff :Files %:p:h<CR>
" file manager
nnoremap <Leader>fn :NnnPicker '%:p:h'<CR>
" tree
nnoremap <Leader>ft :NERDTreeToggle<CR>
