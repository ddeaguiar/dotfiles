behave xterm


let mapleader = "\<SPACE>"

let Tlist_Ctags_Cmd="/usr/local/bin/ctags"

nnoremap ; :

set nocompatible                  " Must come first because it changes other options.

filetype off                      " Required by vundle
set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

"Plugins
Plugin 'tpope/vim-fugitive'
Plugin 'wincent/Command-T'
Plugin 'pangloss/vim-simplefold'
Plugin 'tpope/vim-git'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
Plugin 'altercation/vim-colors-solarized'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'kchmck/vim-coffee-script'
Plugin 'godlygeek/tabular'
Plugin 'LustyExplorer'
Plugin 'LustyJuggler'
Plugin 'matchit.zip'
Plugin 'open-browser.vim'
Plugin 'The-NERD-Commenter'
Plugin 'ack.vim'
Plugin 'VimClojure'
Plugin 'taglist.vim'

call vundle#end()
filetype plugin indent on         " Turn on file type detection.
set omnifunc=syntaxcomplete#Complete
syntax enable                     " Turn on syntax highlighting.

runtime macros/matchit.vim        " Load the matchit plugin.

nmap <silent> <leader>ev :e $MYVIMRC<CR>
" Source the vimrc file after saving it
if has("autocmd")
  autocmd bufwritepost .vimrc source $MYVIMRC
endif


set showcmd                       " Display incomplete commands.
set showmode                      " Display the mode you're in.

set backspace=indent,eol,start    " Intuitive backspacing.

set hidden                        " Handle multiple buffers better.

set wildmenu                      " Enhanced command line completion.
set wildmode=list:longest         " Complete files like a shell.
set wildignore=*.swp,*.bak,*.orig,*.jpg,*.gif,*.png,*.swf,*.fla,*.o,.git,.svn,files/**,sites/default/files/**,backup/modules/**,no-deploy/**,sites/all/modules/ncl_endeca/docs/**,sites/all/themes/norway/html_mockups/**
set ignorecase                    " Case-insensitive searching.
set smartcase                     " But case-sensitive if expression contains a capital letter.

set number                        " Show line numbers.
set ruler                         " Show cursor position.

set incsearch                     " Highlight matches as you type.
set hlsearch                      " Highlight matches.

set wrap                          " Turn on line wrapping.
set scrolloff=3                   " Show 3 lines of context around the cursor.

set title                         " Set the terminal's title

set visualbell                    " No beeping.

set nobackup                      " Don't make a backup before overwriting a file.
set nowritebackup                 " And again.
set noswapfile                    " no swap files
set directory=$HOME/.vim/tmp//,.  " Keep swap files in one location

" UNCOMMENT TO USE
set tabstop=2                    " Global tab width.
set shiftwidth=2                 " And again, related.
set expandtab                    " Use spaces instead of tabs

set laststatus=2                  " Show the status line all the time
" Useful status information at bottom of screen
set statusline=[%n]\ %<%.99f\ %h%w%m%r%y\ %{exists('*CapsLockStatusline')?CapsLockStatusline():''}%=%-16(\ %l,%c-%v\ %)%P%{fugitive#statusline()}

set t_Co=16
let g:solarized_italic=0
set background=dark
colorscheme solarized

" Tab mappings.
map <leader>tt :tabnew<cr>
map <leader>te :tabedit
map <leader>tc :tabclose<cr>
map <leader>to :tabonly<cr>
map <leader>tn :tabnext<cr>
map <leader>tp :tabprevious<cr>
map <leader>tf :tabfirst<cr>
map <leader>tl :tablast<cr>
map <leader>tm :tabmove

" Taglist
nmap <silent> <leader>, :TlistToggle<CR>
nmap <leader>. :tag

"split screen mappings
map <leader>- :split<cr>
map <leader>\ :vsplit<cr>

" cscope
if has("cscope")
  """"""""""""" Standard cscope/vim boilerplate

  " use both cscope and ctag for 'ctrl-]', ':ta', and 'vim -t'
  set cscopetag

  " check cscope for definition of a symbol before checking ctags: set to 1
  " if you want the reverse search order.
  set csto=0

  " add any cscope database in current directory
  if filereadable("cscope.out")
      cs add cscope.out
  " else add the database pointed to by environment variable
  elseif $CSCOPE_DB != ""
      cs add $CSCOPE_DB
  endif

  " show msg when any other cscope db added
  set cscopeverbose


  """"""""""""" My cscope/vim key mappings
  "
  " The following maps all invoke one of the following cscope search types:
  "
  "   's'   symbol: find all references to the token under cursor
  "   'g'   global: find global definition(s) of the token under cursor
  "   'c'   calls:  find all calls to the function name under cursor
  "   't'   text:   find all instances of the text under cursor
  "   'e'   egrep:  egrep search for the word under cursor
  "   'f'   file:   open the filename under cursor
  "   'i'   includes: find files that include the filename under cursor
  "   'd'   called: find functions that function under cursor calls
  "
  " Below are three sets of the maps: one set that just jumps to your
  " search result, one that splits the existing vim window horizontally and
  " diplays your search result in the new window, and one that does the same
  " thing, but does a vertical split instead (vim 6 only).
  "
  " I've used CTRL-\ and CTRL-@ as the starting keys for these maps, as it's
  " unlikely that you need their default mappings (CTRL-\'s default use is
  " as part of CTRL-\ CTRL-N typemap, which basically just does the same
  " thing as hitting 'escape': CTRL-@ doesn't seem to have any default use).
  " If you don't like using 'CTRL-@' or CTRL-\, , you can change some or all
  " of these maps to use other keys.  One likely candidate is 'CTRL-_'
  " (which also maps to CTRL-/, which is easier to type).  By default it is
  " used to switch between Hebrew and English keyboard mode.
  "
  " All of the maps involving the <cfile> macro use '^<cfile>$': this is so
  " that searches over '#include <time.h>" return only references to
  " 'time.h', and not 'sys/time.h', etc. (by default cscope will return all
  " files that contain 'time.h' as part of their name).


  " To do the first type of search, hit 'CTRL-\', followed by one of the
  " cscope search types above (s,g,c,t,e,f,i,d).  The result of your cscope
  " search will be displayed in the current window.  You can use CTRL-T to
  " go back to where you were before the search.
  "

  nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
  nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
  nmap <C-\>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
  nmap <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>


  " Using 'CTRL-spacebar' (intepreted as CTRL-@ by vim) then a search type
  " makes the vim window split horizontally, with search result displayed in
  " the new window.
  "
  " (Note: earlier versions of vim may not have the :scs command, but it
  " can be simulated roughly via:
  "    nmap <C-@>s <C-W><C-S> :cs find s <C-R>=expand("<cword>")<CR><CR>

  nmap <C-@>s :scs find s <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@>g :scs find g <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@>c :scs find c <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@>t :scs find t <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@>e :scs find e <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@>f :scs find f <C-R>=expand("<cfile>")<CR><CR>
  nmap <C-@>i :scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
  nmap <C-@>d :scs find d <C-R>=expand("<cword>")<CR><CR>


  " Hitting CTRL-space *twice* before the search type does a vertical
  " split instead of a horizontal one (vim 6 and up only)
  "
  " (Note: you may wish to put a 'set splitright' in your .vimrc
  " if you prefer the new window on the right instead of the left

  nmap <C-@><C-@>s :vert scs find s <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@><C-@>g :vert scs find g <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@><C-@>c :vert scs find c <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@><C-@>t :vert scs find t <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@><C-@>e :vert scs find e <C-R>=expand("<cword>")<CR><CR>
  nmap <C-@><C-@>f :vert scs find f <C-R>=expand("<cfile>")<CR><CR>
  nmap <C-@><C-@>i :vert scs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
  nmap <C-@><C-@>d :vert scs find d <C-R>=expand("<cword>")<CR><CR>
endif

""""""""""""" key map timeouts
"
" By default Vim will only wait 1 second for each keystroke in a mapping.
" You may find that too short with the above typemaps.  If so, you should
" either turn off mapping timeouts via 'notimeout'.
"
"set notimeout
"
" Or, you can keep timeouts, by uncommenting the timeoutlen line below,
" with your own personal favorite value (in milliseconds):
"
set timeoutlen=2000
"
" Either way, since mapping timeout settings by default also set the
" timeouts for multicharacter 'keys codes' (like <F1>), you should also
" set ttimeout and ttimeoutlen: otherwise, you will experience strange
" delays as vim waits for a keystroke after you hit ESC (it will be
" waiting to see if the ESC is actually part of a key code like <F1>).
"
set ttimeout
"
" personally, I find a tenth of a second to work well for key code
" timeouts. If you experience problems and have a slow terminal or network
" connection, set it higher.  If you don't set ttimeoutlen, the value for
" timeoutlent (default: 1000 = 1 second, which is sluggish) is used.
"
set ttimeoutlen=100


" Map Git
"nmap <leader>gt :Gist
"nmap <leader>gp :Gist -p
nmap <leader>gi :Git
nmap <leader>gl :Glog<CR>
nmap <leader>gd :Gdiff<CR>
nmap <leader>gb :Gblame<CR>
nmap <leader>gc :Gcommit<CR>
nmap <leader>gs :Gstatus<CR>

" Fix searches
nnoremap / /\v
vnoremap / /\v

" Misc Mappings
nnoremap <leader><space> :noh<cr>
"nnoremap <leader>a :Ack
nnoremap <leader>b :buffers<CR>
"nnoremap <leader>pt :!phake test<CR>
nnoremap <silent> <leader>rn :set relativenumber<CR>
cmap w!! w !sudo tee % >/dev/null

autocmd FileType php set keywordprg=phpman
nmap <leader>k :OpenBrowser <C-R>=expand("http://api.drupal.org/api/function/<cword>/6")<CR><CR>

if has('autocmd')
  autocmd FileType python,coffee set expandtab
  autocmd BufRead,BufNewFile *.scss set filetype=scss
  autocmd FileType mail nmap <leader>A :w<CR>:!aspell -e -c %<CR>:e<CR>
  "dash rocket
  autocmd FileType php,coffee imap <C-d> ->
  "hash rocket
  autocmd FileType php,coffee imap <C-f> =>

  augroup module
    autocmd BufRead,BufNewFile *.install set filetype=php
    autocmd BufRead,BufNewFile *.module set filetype=php
    autocmd BufRead,BufNewFile *.inc set filetype=php
  augroup END
endif

if exists(":Tabularize")
  nmap <Leader>a= :Tabularize /=<CR>
  vmap <Leader>a= :Tabularize /=<CR>
  nmap <Leader>a: :Tabularize /:\zs<CR>
  vmap <Leader>a: :Tabularize /:\zs<CR>
endif

let Tlist_Exit_OnlyWindow = 1

let tlist_clojure_settings = 'lisp;f:function'
let vimclojure#HighlightBuiltins = 1
let vimclojure#ParenRainbow=1
" Bubble single lines
"nnoremap <C-Up> [e
"nnoremap <C-Down> ]e
"" Bubble multiple lines
"vnoremap <C-Up> [egv
"vnoremap <C-Down> ]egv

" Automatic fold settings for specific files. Uncomment to use.
" autocmd FileType ruby setlocal foldmethod=syntax
" autocmd FileType css  setlocal foldmethod=indent shiftwidth=2 tabstop=2