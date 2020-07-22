let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
let g:vimtex_fold_enabled=1

nnoremap <Leader>mc :VimtexCompile<CR>
nnoremap <Leader>mt :VimtexTocToggle<CR>
nnoremap <Leader>ml :VimtexLog<CR>

" Conceal
set conceallevel=2
let g:tex_conceal='abdmgs'
let g:tex_conceal_frac=1
" Some fonts may give unreadable glyphs for some super/subscripts
let g:tex_superscripts="[0-9a-zA-W]"
let g:tex_subscripts="[0-9ijklmn+-]"

setlocal spell
set spelllang=en_gb
