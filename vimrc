""
"" Thanks:
""   Gary Bernhardt  <destroyallsoftware.com>
""   Tim Pope        <tbaggery.com>
""   Drew Neil       <vimcasts.org>
""   Mislav Marohnić <http://mislav.uniqpath.com>

source ~/dotfiles/bundle.vim
runtime macros/matchit.vim  " enables % to cycle through `if/else/endif`

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" BASIC EDITING CONFIGURATION
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set encoding=utf-8

syntax enable
set synmaxcol=800           " don't try to highlight long lines

set ruler       " show the cursor position all the time
set cursorline  " highlight the line of the cursor
set showcmd     " show partial commands below the status line
set shell=bash  " avoids munging PATH under zsh
let g:is_bash=1 " default shell syntax
set history=200 " remember more Ex commands
set scrolloff=3 " have some context around the current line always on screen

" Allow backgrounding buffers without writing them, and remember marks/undo
" for backgrounded buffers
set hidden

" Auto-reload buffers when file changed on disk
set autoread

" Disable swap files; systems don't crash that often these days
set updatecount=0

" Make Vim able to edit crontab files again.
set backupskip=/tmp/*,/private/tmp/*"

" Allow to revert changes after vim has closed
set undodir=$HOME/.vim/undo
set undolevels=1000
set undoreload=10000

"" Whitespace
set nowrap                        " don't wrap lines
set tabstop=2                     " a tab is two spaces
set shiftwidth=2                  " an autoindent (with <<) is two spaces
set expandtab                     " use spaces, not tabs
set list                          " Show invisible characters
set backspace=indent,eol,start    " backspace through everything in insert mode

" Joining lines
if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j            " Delete comment char when joining commented lines
endif
set nojoinspaces                  " Use only 1 space after "." when joining lines, not 2

" Indicator chars
" set listchars=tab:▸\ ,trail:•,extends:❯,precedes:❮
set listchars=tab:▸\ ,trail:•,extends:❯,precedes:❮
set showbreak=↪\

"" Searching
set hlsearch                      " highlight matches
set incsearch                     " incremental searching
set ignorecase                    " searches are case insensitive...
set smartcase                     " ... unless they contain at least one capital letter
set gdefault                      " have :s///g flag by default on

" Time out on key codes but not mappings.
" Basically this makes terminal Vim work sanely.
set notimeout
set ttimeout
set ttimeoutlen=100

" Setup defaults for splits
set splitright
set splitbelow

" ignore Rubinius, Sass cache files
set wildignore+=tmp/**,*.rbc,.rbx,*.scssc,*.sassc
" ignore Bundler standalone/vendor installs & gems
set wildignore+=bundle/**,vendor/bundle/**,vendor/cache/**
set wildignore+=node_modules/**

" Set leader to ,
let mapleader=","

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CUSTOM AUTOCMDS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("autocmd")
  " Avoid showing trailing whitespace when in insert mode
  au InsertEnter * :set listchars-=trail:•
  au InsertLeave * :set listchars+=trail:•

  " In Makefiles, use real tabs, not tabs expanded to spaces
  au FileType make set noexpandtab
  " In gitconfig, use real tabs, not tabs expanded to spaces
  au FileType gitconfig set noexpandtab

  " Make sure all markdown files have the correct filetype set and setup wrapping
  au BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn,txt} setf markdown | call s:setupWrapping()

  " Treat JSON files like JavaScript
  au BufNewFile,BufRead *.json set ft=javascript

  " https://github.com/sstephenson/bats
  au BufNewFile,BufRead *.bats set ft=sh

  " make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
  au FileType python set softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79

  " Remember last location in file, but not for commit messages.
  " see :help last-position-jump
  au BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g`\"" | endif

  " mark Jekyll YAML frontmatter as comment
  au BufNewFile,BufRead *.{md,markdown,html,xml} sy match Comment /\%^---\_.\{-}---$/

  " magic markers: enable using `H/S/J/C to jump back to
  " last HTML, stylesheet, JS or Ruby code buffer
  au BufLeave *.{erb,html}      exe "normal! mH"
  au BufLeave *.{css,scss,sass} exe "normal! mS"
  au BufLeave *.{js,coffee}     exe "normal! mJ"
  au BufLeave *.{rb}            exe "normal! mC"
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" COLOR
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set background=dark
color solarized

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" STATUS LINE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("statusline") && !&cp
  set laststatus=2              " always show the status bar
  set statusline=%f\ %m\ %r     " filename, modified, readonly
  set statusline+=%{fugitive#statusline()}
  set statusline+=\ %l/%L[%p%%] " current line/total lines
  set statusline+=\ %v[0x%B]    " current column [hex char]
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MISC KEY MAPS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" copy to clipboard clipboard
map <leader>y "*y

" Switch paste mode with F2 to quickly disable/enable indenting for paste.
set pastetoggle=<F13>

" Jump to next/previous screen row in the editor instead of the line when wrapping.
nmap j gj
nmap k gk

" Use Cmd + N to change tabs
map <D-1> 1gt
map <D-2> 2gt
map <D-3> 3gt
map <D-4> 4gt
map <D-5> 5gt
map <D-6> 6gt
map <D-7> 7gt
map <D-8> 8gt
map <D-9> 9gt
map <D-0> :tablast<CR>

" Move around splits with <c-hjkl>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" toggle between last open buffers
nnoremap <leader><leader> <c-^>

" paste lines from unnamed register and fix indentation
nmap <leader>p pV`]=
nmap <leader>P PV`]=

" don't use Ex mode, use Q for formatting
map Q gq

" clear the search buffer when hitting return
:nnoremap <CR> :nohlsearch<cr>

" toggle the current fold
:nnoremap <Space> za

" In command-line mode, C-a jumps to beginning (to match C-e)
cnoremap <C-a> <Home>

" double percentage sign in command mode is expanded
" to directory of current file - http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" Close all other windows, open a vertical split, and open this file's test
" alternate in it.
nnoremap <leader>s :call FocusOnFile()<cr>
" nnoremap <leader>s <c-w>o <c-w>v <c-w>w :call OpenTestAlternate()<cr>
nnoremap <leader>s :call FocusOnFile()<cr>
function! FocusOnFile()
  tabnew %
  normal! v
  normal! l
  call OpenTestAlternate()
  normal! h
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ARROW KEYS ARE UNACCEPTABLE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CTRLP KEY MAPS AND CONFIGURATION
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ACK KEY MAPS AND CONFIGURATION
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ackprg = 'ag --nogroup --nocolor --column'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NERDTREE KEY MAPS AND CONFIGURATION
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Map toggling NERDTree to <C-n>
map <C-n> :NERDTreeToggle<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PRESERVE HISTORY
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Preserve history and cursor position while executing the given command
function! Preserve(command)
  " Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the business:
  execute a:command
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" KILL WHITE SPACE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! StripTrailingWhitespaces()
  call Preserve("%s/\\s\\+$//e")
endfunction
nnoremap <silent> <F5> :call StripTrailingWhitespaces()<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MARKDOWN WRAPPING
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function s:setupWrapping()
  set wrap
  set wrapmargin=2
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SWITCH BETWEEN TEST AND PRODUCTION CODE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! OpenTestAlternate()
  let new_file = AlternateForCurrentFile()
  exec ':e ' . new_file
endfunction
function! AlternateForCurrentFile()
  let current_file = expand("%")
  let new_file = current_file
  let in_spec = match(current_file, '^spec/') != -1
  let going_to_spec = !in_spec
  let in_app = match(current_file, '\<controllers\>') != -1 || match(current_file, '\<models\>') != -1 || match(current_file, '\<views\>') != -1 || match(current_file, '\<helpers\>') != -1
  if going_to_spec
    if in_app
      let new_file = substitute(new_file, '^app/', '', '')
    end
    let new_file = substitute(new_file, '\.e\?rb$', '_spec.rb', '')
    let new_file = 'spec/' . new_file
  else
    let new_file = substitute(new_file, '_spec\.rb$', '.rb', '')
    let new_file = substitute(new_file, '^spec/', '', '')
    if in_app
      let new_file = 'app/' . new_file
    end
  endif
  return new_file
endfunction
nnoremap <leader>. :call OpenTestAlternate()<cr>
