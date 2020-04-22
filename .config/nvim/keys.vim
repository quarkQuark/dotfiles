" Use jk as escape
inoremap jk <esc>

" Visual line movement
nnoremap j gj
nnoremap k gk

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
