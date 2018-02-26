" Set colorScheme variable for use in other settings
let colorSch="solarized"
let backgroundColor="dark"
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
set ignorecase
set smartcase

" Highlight search results. Use :noh to undo
" set hlsearch
" Enables some basic mouse input
set mouse=a
set mousemodel="popup_setpos"
" Show line numbers
set number
" Line numbers are displayed relative to current line.
set relativenumber
" Hide buffer (don't ask for save) when navigating away.
set hidden
" Don't save hidden and unloaded buffers in sessions.
set sessionoptions-=buffers
set wildmenu
set wildmode=list:longest
set scrolloff=2
" Tabs, trailing ws visible
set listchars=tab:>-,trail:·,eol:$
set autowrite       " Automatically save before commands like :next and :make
set complete-=i     " Searching includes can be slow
set display=lastline
set diffopt+=vertical

let s:vimrcdir = fnamemodify(expand("$MYVIMRC"), ":p:h")

" Automatically create vimfile directories in same location as vimrc
function! CreateVimDir(dir)
    let l:dir = fnameescape(expand(a:dir))
    if filewritable(s:vimrcdir) && !isdirectory(s:vimrcdir . l:dir)
        mkdir(s:vimrcdir . l:dir)
    endif
    return s:vimrcdir . l:dir
endfunction
call CreateVimDir("/vimfiles")

" Create undo file for inter-session undo
" Extra slash means files will have unique names
set undofile
exec 'set undodir=' . CreateVimDir("/vimfiles/undo") . '/'
" Save on focus loss
au FocusLost * :wa

exec 'set backupdir=' . CreateVimDir("/vimfiles/backup") . '/'
exec 'set directory=' . CreateVimDir("/vimfiles/swap") . '/'
" ,"$TMP//","$TEMP//",.//

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
    let backgroundColor="light"
    exec 'set background=' . backgroundColor
    if has ("win32") || has ("gui_macvim")
        set guifont=Source\ Code\ Pro\ Medium:h11
    else
        set guifont=Source\ Code\ Pro\ Medium\ 11
    endif
        " Put buffer name in window title, without "Vim" (because it'll have a logo)
        autocmd BufEnter * let &titlestring = '' . expand("%:t") . ' [' . expand("%:p") . ']'
        set title
    else
    " This is console Vim.
    exec "let g:".colorSch . "_termcolors=&t_Co"
    exec "let g:".colorSch . "_termtrans=1"
    " if exists("+lines")
        " set lines=30
    " endif
    " if exists("+columns")
        " set columns=100
    " endif
    " Put buffer name in window title
    autocmd BufEnter * let &titlestring = '|Vim| ' . expand("%:t") . ' [' . expand("%:p") . ']'
    set title
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

