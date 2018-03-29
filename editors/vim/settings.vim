" Set colorScheme variable for use in other settings
" Doesn't override preset scheme
" Background should always be set after colorscheme.
if exists('&g:colors_name')
    let colorSch = g:colors_name
endif
if !exists ('&colorSch')
    let colorSch="solarized"
endif
if exists ('&background')
    let backgroundColor = &background
endif

set encoding=utf-8
filetype plugin indent on
" For highlighting, and color schemes
syntax on

if has("gui_running")
    " GUI is running or is about to start.
    " Remove menus to speed up startup
    set guioptions=M
    " Maximize gvim window (for an alternative on Windows, see simalt
    "   below).
    set lines=40 columns=120
    if !exists ('&backgroundColor')
        let backgroundColor="light"
    endif
    if colorSch == ""
        let backgroundColor="light"
    endif
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

    " Console Vim settings
 
    " If the current iTerm tab has been
    " created using the **dark** profile:
    if $ITERM_PROFILE == 'Solarized Dark'
      set background=dark
    endif
    " If the current iTerm tab has been
    " created using the **light** profile:
    if $ITERM_PROFILE == 'Solarized Light'
      set background=light
    endif

    if !exists ('&backgroundColor')
        let backgroundColor="dark"
    endif

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
set sessionoptions-=blank
set wildmenu
set wildmode=longest,list:longest
set scrolloff=5
set listchars=tab:>-,trail:·,eol:¬,precedes:←,extends:→,nbsp:·
set complete-=i     " Searching includes can be slow
set display=lastline
set diffopt+=vertical

" set viminfo +=

let s:vimrcdir = fnamemodify($MYVIMRC, ":p:h")
let s:viminfoPath=s:vimrcdir . expand("/vimfiles/viminfo")
let s:viminfoPath=substitute(s:viminfoPath,'\\','/','g')
exec 'set viminfo+=n' . fnameescape(s:viminfoPath)

exec 'set backupdir=' . CreateVimDir("vimfiles/backup") . expand('/')
exec 'set directory=' . CreateVimDir("vimfiles/swap") . expand('/')
exec 'set undodir=' . CreateVimDir("vimfiles/undo") . expand('/')
" Create undo file for inter-session undo
" Extra slash means files will have unique names
set undofile
" Don't autosave if there is no buffer name.
if bufname('%') != ''
    " Automatically save before commands like :next and :make
    set autowrite
    augroup autowrite
        " Save on focus loss
        au FocusLost * :wa
        " Save leaving insert
        au InsertLeave <buffer> :update
    augroup END
endif

set expandtab
set shiftwidth=4
set softtabstop=4
set smarttab
" set tabstop=4
" See :h fo-table. Wrapping and joingin options.
set formatoptions +=lj

set modeline
set modelines=5
set wrap
" Colour the 80th column
set colorcolumn=80
" Linebreak relates to when a line wraps (ie not in a word)
set linebreak
" backspace and cursor keys wrap to previous/next line
set backspace=indent,eol,start whichwrap+=<,>,[,]
" Vertical window splits open on right side, horizontal below
set splitright
set splitbelow
set shortmess=a
set cmdheight=2
set laststatus=2
" Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor = "latex"

" No annoying sound on errors
set noerrorbells
" set novisualbell
" set t_vb=
" set tm=500

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

"highlight whitespace at the ends of lines
" autocmd InsertEnter * syn clear EOLWS | syn match EOLWS excludenl /\s\+\%#\@!$/
" autocmd InsertLeave * syn clear EOLWS | syn match EOLWS excludenl /\s\+$/
" highlight EOLWS ctermbg=red guibg=red

" Autoset new buffers to scratch
augroup scratch
    autocmd!
    autocmd BufEnter * if &filetype == "" | setlocal ft=scratch | endif
    autocmd BufEnter * if &filetype == "" | setlocal spell | setl ai
augroup END

" Set spellfile to location that is guaranteed to exist, can be symlinked to Dropbox or kept in Git and managed outside of thoughtbot/dotfiles using rcm.
" set spellfile=$HOME/.vim-spell-en.utf-8.add

" Set cursor based on mode. Designed for iTerm, may be different for others.
if empty($TMUX)
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  let &t_SR = "\<Esc>]50;CursorShape=2\x7"
else
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  let &t_SR = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=2\x7\<Esc>\\"
endif

augroup misc
    autocmd!
    " Auto cd to working dir of this window's file
    " autocmd BufEnter * silent! lcd %:p:h
augroup END
