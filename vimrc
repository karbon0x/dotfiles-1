""
"" Thanks:
""   Gary Bernhardt  <destroyallsoftware.com>
""   Tim Pope        <tbaggery.com>
""   Drew Neil       <vimcasts.org>
""   Mislav Marohnić <mislav.uniqpath.com>
""   Steve Losh      <learnvimscriptthehardway.stevelosh.com>

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
set listchars=tab:>\ ,trail:*

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
  " Autocmd group responsible for file type specific autocmd's
  augroup file_types
    " Clear out the file_types autocommand group
    autocmd!

    " In Makefiles, use real tabs, not tabs expanded to spaces
    autocmd FileType make setlocal noexpandtab

    " In gitconfig, use real tabs, not tabs expanded to spaces
    autocmd FileType gitconfig setlocal noexpandtab

    " Make groovy play nice. And :( groovy
    " Set 'formatoptions' to break comment lines but not other lines,
    " and insert the comment leader when hitting <CR> or using "o".
    " Set 'comments' to format dashed lists in comments. Behaves just like C.
    autocmd FileType groovy setlocal formatoptions-=t formatoptions+=croql
                              \ smartindent autoindent
                              \ comments& comments^=sO:*\ -,mO:*\ \ ,exO:*/
                              \ commentstring=//%s

    " Setup dispatch to default to running the current test file
    autocmd FileType groovy let b:dispatch = 'mvn test -Dtest=' . expand("%:t:r")
    autocmd FileType java let b:dispatch = 'mvn test -Dtest=' . expand("%:t:r")

    " make Python follow PEP8 ( http://www.python.org/dev/peps/pep-0008/ )
    autocmd FileType python setlocal softtabstop=4 tabstop=4 shiftwidth=4 textwidth=79

    " Setup dispatch to default to running the current test file
    autocmd FileType ruby let b:dispatch = 'rspec %'
  augroup END

  " Autocmd group responsible for buffer opening and closing specific
  " autocmd's
  augroup event_group
    " Clear out the event_group autocommand group
    autocmd!

    " Make sure all markdown files have the correct filetype set and setup wrapping
    autocmd BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn,txt} setf markdown | call s:setupWrapping()

    " Treat JSON files like JavaScript
    autocmd BufRead,BufNewFile *.json set ft=javascript

    " https://github.com/sstephenson/bats
    autocmd BufRead,BufNewFile *.bats set ft=sh

    " Set the filetype to rust when opening a rust file
    autocmd BufRead,BufNewFile *.rs set ft=rust

    " Remember last location in file, but not for commit messages.
    " see :help last-position-jump
    autocmd BufReadPost * if &filetype !~ '^git\c' && line("'\"") > 0 && line("'\"") <= line("$")
      \| exe "normal! g`\"" | endif

    " mark Jekyll YAML frontmatter as comment
    autocmd BufRead,BufNewFile *.{md,markdown,html,xml} sy match Comment /\%^---\_.\{-}---$/

    " magic markers: enable using `H/S/J/C to jump back to
    " last HTML, stylesheet, JS or Ruby code buffer
    autocmd BufLeave *.{erb,html}      exe "normal! mH"
    autocmd BufLeave *.{css,scss,sass} exe "normal! mS"
    autocmd BufLeave *.{js,coffee}     exe "normal! mJ"
    autocmd BufLeave *.{rb}            exe "normal! mC"
  augroup END

  " Autcomd group responsible for leaving and entering insert mode specific
  " autocmd's
  augroup insert_mode_group
    " Avoid showing trailing whitespace when in insert mode
    autocmd InsertEnter * :set listchars-=trail:*
    autocmd InsertLeave * :set listchars+=trail:*
  augroup END
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
  " set statusline=%f\ %m\ %r     " filename, modified, readonly
  " set statusline+=%{fugitive#statusline()}
  " set statusline+=\ %l/%L[%p%%] " current line/total lines
  " set statusline+=\ %v[0x%B]    " current column [hex char]
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VIM-STATUSLINE CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline_detect_modified=1
let g:airline_detect_paste=1
let g:airline_detect_whitespace=1 "icon and message (default)
let g:airline_exclude_preview = 0
let g:airline_theme='solarized'

let g:airline_enable_branch = 1
let g:airline_branch_empty_message = ''

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CUSTOM KEY MAPS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Open vimrc for editing
nnoremap <leader>ev :vsplit $MYVIMRC<cr>

" Load new changes made in vimrc
nnoremap <leader>sv :source $MYVIMRC<cr>

" copy to clipboard
noremap <leader>y "*y

" paste from clipboard
noremap <leader>p "*P

" paste lines from unnamed register and fix indentation
" nnoremap <leader>p pV`]=
" nnoremap <leader>P PV`]=

" Operator pending mapping that effects the content of the surround
" parentheses. Think of this as 'parentheses'
" Example dp => 'delete parameters'
onoremap p i(

" Operator pending mapping that effects the content of the next set of
" surrounding parentheses. Think of this as 'inside next parentheses'.
" Example cin( => 'change inside next parentheses'
onoremap in( :<c-u>normal! f(vi(<cr>

" Operator pending mapping that effects the content of the last set of
" surrounding parentheses. Think of this as 'inside last parentheses'.
" Example cin( => 'change inside last parentheses'
onoremap il( :<c-u>normal! F)vi(<cr>

" Switch paste mode with F16 to quickly disable/enable indenting for paste.
set pastetoggle=<F16>

" Jump to next/previous screen row in the editor instead of the line when wrapping.
nnoremap j gj
nnoremap k gk

" Use Cmd + N to change tabs
noremap <D-1> 1gt
noremap <D-2> 2gt
noremap <D-3> 3gt
noremap <D-4> 4gt
noremap <D-5> 5gt
noremap <D-6> 6gt
noremap <D-7> 7gt
noremap <D-8> 8gt
noremap <D-9> 9gt
noremap <D-0> :tablast<CR>

" toggle between last open buffers
nnoremap <leader><leader> <c-^>

" don't use Ex mode, use Q for formatting
noremap Q gq

" clear the search buffer when hitting return
nnoremap <CR> :nohlsearch<cr>

" toggle the current fold
nnoremap <Space> za

" In command-line mode, C-a jumps to beginning (to match C-e)
cnoremap <C-a> <Home>

" double percentage sign in command mode is expanded
" to directory of current file - http://vimcasts.org/e/14
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" Close all other windows, open a vertical split, and open this file's test
" alternate in it.
nnoremap <leader>s :call FocusOnFile()<cr>

" Open the current file's test alternate in a vertical split.
nnoremap <leader>. :call OpenTestAlternate()<cr>

" Run dispatch
nnoremap <leader>t :Dispatch<CR>  

" Rename the current buffers file and prompt for the new file name.
nnoremap <leader>n :call RenameFile()<cr>

" Strip the whitespace from the current buffer.
map <silent> <leader>w :call StripTrailingWhitespaces()<CR>

" Map toggling NERDTree to <C-n>
noremap <C-n> :NERDTreeToggle<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ARROW KEYS ARE UNACCEPTABLE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>
nnoremap <Up> <Nop>
nnoremap <Down> <Nop>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HITTING ESC IS UNACCEPTABLE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
inoremap <C-[> <esc>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" HITTING DELETE IS UNACCEPTABLE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
inoremap <Del> <Nop>
inoremap <BS>  <Nop>

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
" NERDTREE CONFIGURATION
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let NERDTreeShowHidden = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" TURBUX CONFIGURATIONS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MARKDOWN WRAPPING
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:setupWrapping()
  set wrap
  set wrapmargin=2
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" FOCUS ON CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! FocusOnFile()
  tabnew %
  normal! v
  normal! l
  call OpenTestAlternate()
  normal! h
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RENAME CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'), 'file')
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" SWITCH BETWEEN TEST AND PRODUCTION CODE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! OpenTestAlternate()
  let new_file = AlternateForCurrentFile()
  exec ':vsp ' . new_file
endfunction

function! AlternateForCurrentFile()
  let current_file = expand("%")
  let new_file = current_file
  if match(current_file, '\.rb$') != -1
    let new_file = RubyAlternateForCurrentFile(current_file)
  elseif match(current_file, '\.java$') != -1
    let new_file = JavaAlternateForCurrentFile(current_file)
  elseif match(current_file, '\.groovy$') != -1
    let new_file = GroovyAlternateForCurrentFile(current_file)
  endif
  return new_file
endfunction

function! RubyAlternateForCurrentFile(current_file)
  let new_file = a:current_file
  let in_spec = match(a:current_file, '^spec/') != -1
  let going_to_spec = !in_spec
  let in_app = match(a:current_file, '\<controllers\>') != -1 || match(a:current_file, '\<models\>') != -1 || match(a:current_file, '\<views\>') != -1 || match(a:current_file, '\<helpers\>') != -1 || match(a:current_file, '\<presenters\>') != -1
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

function! JavaAlternateForCurrentFile(current_file)
  let new_file = a:current_file
  let in_test = match(a:current_file, 'Test\.java$') != -1
  let going_to_test = !in_test
  if going_to_test
    let new_file = substitute(new_file, '\.java$', 'Test\.java', '')
    let new_file = substitute(new_file, '\/main\/', '\/test\/', '')
  else
    let new_file = substitute(new_file, 'Test\.java$', '\.java', '')
    let new_file = substitute(new_file, '\/test\/', '\/main\/', '')
  endif
  return new_file
endfunction

function! GroovyAlternateForCurrentFile(current_file)
  let new_file = a:current_file
  let in_test = match(a:current_file, 'Test\.groovy$') != -1
  let going_to_test = !in_test
  if going_to_test
    let new_file = substitute(new_file, '\.groovy$', 'Test\.groovy', '')
    let new_file = substitute(new_file, '\/main\/', '\/test\/', '')
  else
    let new_file = substitute(new_file, 'Test\.groovy$', '\.groovy', '')
    let new_file = substitute(new_file, '\/test\/', '\/main\/', '')
  endif
  return new_file
endfunction
