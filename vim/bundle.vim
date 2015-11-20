""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VUNDLE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible
filetype off
" Vundle setup
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let vundle manage vundle
Plugin 'gmarik/Vundle.vim'

Plugin 'powerline/powerline', {'rtp': 'powerline/bindings/vim/'}

Plugin 'mileszs/ack.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'kien/ctrlp.vim'
Plugin 'vim-scripts/groovy.vim'
Plugin 'Yggdroot/indentLine'
Plugin 'tomasr/molokai'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
" Plugin 'scrooloose/syntastic'
Plugin 'majutsushi/tagbar'
Plugin 'godlygeek/tabular'
Plugin 'tpope/vim-bundler'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-haml'
Plugin 'pangloss/vim-javascript'
Plugin 'sleistner/vim-jshint'
Plugin 'groenewege/vim-less'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'xolox/vim-misc'
Plugin 'beloglazov/vim-online-thesaurus'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-repeat'
Plugin 'vim-ruby/vim-ruby'
Plugin 'derekwyatt/vim-scala'
Plugin 'tpope/vim-surround'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'tpope/vim-unimpaired'
Plugin 'stephpy/vim-yaml'
Plugin 'Valloric/YouCompleteMe'
Plugin 'rust-lang/rust.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
