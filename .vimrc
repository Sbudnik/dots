execute pathogen#infect ()

syntax on
colorscheme onedark

source $VIMRUNTIME/vimrc_example.vim

" Use the internal diff if available.
" Otherwise use the special 'diffexpr' for Windows.
if &diffopt !~# 'internal'
  set diffexpr=MyDiff()
endif
function MyDiff()
  let opt = '-a --binary '
  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  let arg1 = v:fname_in
  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  let arg1 = substitute(arg1, '!', '\!', 'g')
  let arg2 = v:fname_new
  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  let arg2 = substitute(arg2, '!', '\!', 'g')
  let arg3 = v:fname_out
  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  let arg3 = substitute(arg3, '!', '\!', 'g')
  if $VIMRUNTIME =~ ' '
    if &sh =~ '\<cmd'
      if empty(&shellxquote)
        let l:shxq_sav = ''
        set shellxquote&
      endif
      let cmd = '"' . $VIMRUNTIME . '\diff"'
    else
        B
      let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
    endif
  else
    let cmd = $VIMRUNTIME . '\diff'
  endif
  let cmd = substitute(cmd, '!', '\!', 'g')
  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3
  if exists('l:shxq_sav')
    let &shellxquote=l:shxq_sav
  endif
endfunction

set nocompatible

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


set t_u7=
set ambw=double

nnoremap <leader>html :-1read $HOME/.vim/.skeleton.html<CR>5jwf>a
noremap <Leader>y "*y
noremap <Leader>p "*p
