set runtimepath+=/usr/share/vim/vimfiles

call plug#begin('/etc/xdg/nvim/plugged')
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'jiangmiao/auto-pairs'
call plug#end()

nmap <silent> q :q! <cr>

let g:airline_powerline_fonts = 1
\"let g:airline_theme='deus'
let g:airline_theme='bubblegum'
\"let g:airline_theme='minimalist'
set nu
