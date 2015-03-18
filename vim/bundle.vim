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

Bundle 'powerline/powerline', {'rtp': 'powerline/bindings/vim/'}

Bundle 'mileszs/ack.vim'
Bundle 'altercation/vim-colors-solarized'
Bundle 'kien/ctrlp.vim'
Bundle 'vim-scripts/groovy.vim'
Bundle 'Yggdroot/indentLine'
Bundle 'tomasr/molokai'
Bundle 'scrooloose/nerdtree'
" Bundle 'scrooloose/syntastic'
Bundle 'majutsushi/tagbar'
Bundle 'godlygeek/tabular'
Bundle 'tpope/vim-bundler'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-fugitive'
Bundle 'airblade/vim-gitgutter'
Bundle 'tpope/vim-haml'
Bundle 'pangloss/vim-javascript'
Bundle 'sleistner/vim-jshint'
Bundle 'groenewege/vim-less'
Bundle "terryma/vim-multiple-cursors"
Bundle 'xolox/vim-misc'
Bundle 'beloglazov/vim-online-thesaurus'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-repeat'
Bundle 'vim-ruby/vim-ruby'
Bundle 'wting/rust.vim'
Bundle 'derekwyatt/vim-scala'
Bundle 'tpope/vim-surround'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'tpope/vim-unimpaired'
Bundle 'stephpy/vim-yaml'
Bundle 'Valloric/YouCompleteMe'
