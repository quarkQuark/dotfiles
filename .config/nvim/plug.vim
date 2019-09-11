""""""""""""""""""""""""""""""""""""""""
"" => NERDTree
""""""""""""""""""""""""""""""""""""""""

" Start automatically when vim is started with no arguments
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree

let NERDTreeQuitOnOpen = 1
let NERDTreeDirArrows = 1

" Keybinding
nnoremap <Leader>f :NERDTreeToggle<CR>
