""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VUNDLE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible
filetype off
" Vundle setup
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
filetype plugin indent on

Bundle 'mileszs/ack'
Bundle 'kien/ctrlp.vim'
" Bundle 'othree/html5.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'godlygeek/tabular'
Bundle 'bling/vim-airline'
Bundle 'tpope/vim-bundler'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-haml'
Bundle 'pangloss/vim-javascript'
Bundle 'sleistner/vim-jshint'
Bundle 'tpope/vim-liquid'
Bundle 'nelstrom/vim-markdown-folding'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'vim-ruby/vim-ruby'
Bundle 'wting/rust.vim'
" Bundle 'tpope/vim-rvm'
Bundle 'tpope/vim-surround'
Bundle 'christoomey/vim-tmux-navigator'
" Bundle 'benmills/vimux'
" Bundle 'jgdavey/vim-turbux'
Bundle 'tpope/vim-unimpaired'

" Color themes
Bundle 'tomasr/molokai'
Bundle 'altercation/vim-colors-solarized'

" Vim scripts
Bundle 'vim-scripts/groovy.vim'
" Bundle 'vim-scripts/java.vim'
