""""""""""""""""""""""""""""""""""""""""
" Plugin stuff
""""""""""""""""""""""""""""""""""""""""
set nocompatible              " be iMproved, required
filetype off                  " required

call plug#begin('~/.config/nvim/plugged')

Plug 'vim-latex/vim-latex'
Plug 'itchyny/lightline.vim'
Plug 'preservim/nerdtree'
Plug 'jeffkreeftmeijer/vim-numbertoggle'
Plug 'ap/vim-css-color'
"Plug 'jalvesaq/Nvim-R', { 'branch': 'stable' }

call plug#end()

"""""""""""""""""""""""""""""""""""""""
" Other settings
"""""""""""""""""""""""""""""""""""""""
let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ }

filetype plugin indent on
set encoding=utf-8
set relativenumber
set nu rnu
syntax on
set termguicolors
set laststatus=2
set clipboard+=unnamedplus
inoremap jj <Esc>
set viminfo=%,<800,'10,/50,:100,h,f0,n~/.cache/nvim/viminfo

" Make nvim indent more like emacs
set expandtab
set autoindent
set smartindent
set shiftwidth=4
set softtabstop=4
set tabstop=8
