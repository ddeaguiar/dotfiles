syntax on
filetype on
filetype plugin on
filetype indent on

let mapleader = ","

set hlsearch
set incsearch
set ts=2
set sw=2
set ai
set number 
set ruler
set nocompatible
set nowrap
set ignorecase
set smartcase
set smarttab
set showcmd
set showmode
set expandtab
set hidden
set shortmess=atI
set visualbell
set scrolloff=3
set wildmenu
set wildmode=list:longest
set showmatch
set foldmethod=syntax
set gfn="Inconsolata/12"
set gfw="Inconsolata/12"
set backspace=indent,eol,start
set history=1000

compiler ruby
colors molokai

autocmd BufNewFile,BufRead COMMIT_EDITMSG set filetype=gitcommit
autocmd FileType make set noexpandtab
autocmd FileType python set noexpandtab

nnoremap ' `
nnoremap ` '

nmap <F2> zR
nmap <F3> za
map <F4> :TlistToggle<CR>
map <F5> :NERDTree<CR>
map <F7> :Rscript<CR>

runtime macros/matchit.vim
