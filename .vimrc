execute pathogen#infect()
syntax on
filetype plugin indent on

colorscheme onedark
highlight Normal ctermbg=None
highlight LineNr ctermfg=DarkGrey

set nocompatible

syntax on
filetype plugin on

let mapleader=" "

map <leader>s :source ~/.vimrc<CR>

set hidden
set history=100

set number relativenumber
set wrap
set encoding=utf-8
set mouse=a
set path+=**
set wildmenu
set lazyredraw
set showmatch
set laststatus=2
set visualbell
set tabstop=4
set expandtab
set shiftwidth=4
set softtabstop=3
set autoindent
set smartindent

set cursorline
hi clear CursorLine
hi CursorLine gui=underline cterm=underline
set autoread
set incsearch
set hlsearch

nnoremap <esc> :noh<return><esc>

let NERDTreeMapActivateNode='<right>'
let NERDTreeShowHidden=1
nmap <leader>n :NERDTreeToggle<CR>
nmap <leader>j :NERDTreeFind<CR>

autocmd VimEnter * NERDTree
autocmd VimEnter * wincmd p

set t_u7=
set ambw=double

nnoremap <leader>html :-1read $HOME/.vim/.skeleton.html<CR>5jwf>a
noremap <Leader>y "*y
noremap <Leader>p "*p

" Get the defaults that most users want.
source $VIMRUNTIME/defaults.vim

if has("vms")
  set nobackup		" do not keep a backup file, use versions instead
else
  set backup		" keep a backup file (restore to previous version)
  if has('persistent_undo')
    set undofile	" keep an undo file (undo changes after closing)
  endif
endif

if &t_Co > 2 || has("gui_running")
  " Switch on highlighting the last used search pattern.
  set hlsearch
endif

" Put these in an autocmd group, so that we can delete them easily.
augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78
augroup END

" Add optional packages.
"
" The matchit plugin makes the % command work better, but it is not backwards
" compatible.
" The ! means the package won't be loaded right away but when plugins are
" loaded during initialization.
if has('syntax') && has('eval')
  packadd! matchit
endif

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline_powerline_fonts = 1
set rtp+=/usr/local/opt/fzf
