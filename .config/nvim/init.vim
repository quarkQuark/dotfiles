" Set spacebar as leader key (must be done first)
let mapleader="\<SPACE>"
nnoremap <SPACE> <Nop>

" Plugin installation and configuration
source $HOME/.config/nvim/plug.vim

" General settings
source $HOME/.config/nvim/conf.vim

" Keybindings
source $HOME/.config/nvim/keys.vim

" Aesthetics and statusline (airline)
source $HOME/.config/nvim/ui.vim

" Filetype-specific configuration (<leader>m):w
" 
au FileType haskell (source $HOME/.config/nvim/filetype/haskell.vim)
