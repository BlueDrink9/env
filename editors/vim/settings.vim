" Set colorScheme variable for use in other settings
let colorSch="solarized"
set encoding=utf-8
set nocompatible

filetype plugin indent on
" For highlighting, and color schemes
syntax on
set showmode
set showcmd
" Show cursor coords
set ruler
" Highligh search as you go
set incsearch
" Ignore case in searches excepted if an uppercase letter is used
set smartcase
" Highlight search results. Use :noh to undo
set hlsearch
" Enables some basic mouse input
set mouse=a
" Show line numbers
set number
" Line numbers are displayed relative to current line.
set relativenumber
" Hide buffer (don't ask for save) when navigating away.
set hidden

let s:undodir = fnamemodify(expand("$MYVIMRC"), ":p:h")
" Create undo file for inter-session undo
set undofile
exec 'set undodir=' . s:undodir . '/vimundo'
" Save on focus loss
au FocusLost * :wa

set expandtab
set shiftwidth=4
set softtabstop=4
set smarttab
" set tabstop=4
"

set modeline
set modelines=5
set wrap
" Colour the 80th column
set colorcolumn=80
" Linebreak relates to when a line wraps (ie not in a word)
set linebreak
" backspace and cursor keys wrap to previous/next line
set backspace=indent,eol,start whichwrap+=<,>,[,]
" Vertical window splits open on right side
set splitright

" No annoying sound on errors
set noerrorbells
" set novisualbell
" set t_vb=
" set tm=500

if has("gui_running")
    " GUI is running or is about to start.
    "   " Maximize gvim window (for an alternative on Windows, see simalt
    "   below).
    set lines=40 columns=120
    set background=light
    set guifont=Source\ Code\ Pro\ Semibold:h10.5
else
    exec "let g:".colorSch . "_termcolors=&t_Co"
    exec "let g:".colorSch . "_termtrans=1"
    " This is console Vim.
    " if exists("+lines")
        " set lines=30
    " endif
    " if exists("+columns")
        " set columns=100
    " endif
endif



" Set up default file explorer plugin to be like NERDTree (for using noplugin
" mode).
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25
" Run on startup
" augroup ProjectDrawer
"     autocmd!
"     autocmd VimEnter * :Vexplore
" augroup END

" Put buffer name in window title
" set titlestring=%t%(\ %M%)%(\ (%{expand(\"%:p:h\")})%)%(\ %a%)\ -\ %{v:servername}
autocmd BufEnter * let &titlestring = '|Vim| ' . expand("%:t") . ' [' . expand("%:p") . ']'
set title
