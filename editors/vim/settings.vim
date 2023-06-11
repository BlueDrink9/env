" vim: set ft=vim:
" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" Folder in which current script resides:
if v:version >= 703
    let s:scriptdir = fnameescape(expand('<sfile>:p:h'))
else
    let s:scriptdir = expand('<sfile>:p:h')
endif

" Needs to be set before plugins use it. Set here rather than in mappings.
let mapleader = " "
let maplocalleader = ""

" Set colorScheme variable for use in other settings
" Doesn't override preset scheme
" Background should always be set after colorscheme.
if exists('&g:colors_name')
    let colorSch = g:colors_name
endif
" Only used if colorSch not set (plugins didn't get loaded)
let g:defaultColorSch="morning"


if
    \ has("gui_running") ||
    \ exists("g:gui_oni") ||
    \ exists("g:started_by_firenvim") ||
    \ exists('g:nvy') ||
    \ exists('g:neovide') ||
    \ exists('g:GuiLoaded')
    let g:hasGUI=1
else
    let g:hasGUI=0
endif

set encoding=utf-8
" Need to add M flag before syntax and filetype, because they source menu.vim.
set guioptions+=M
" For highlighting, and color schemes
" Should go before ft plugin on
syntax on
filetype plugin indent on
" :h syn-sync. Complicated stuff, can't figure it out really.
autocmd myVimrc BufWinEnter,Syntax * syn sync minlines=100 | syn sync maxlines=1000
command! SynSync syntax sync fromstart

" {[} Colours
let s:defaultBGGUI="light"
let s:defaultBGConsole="light"
let s:defaultColorSch="solarized"

" Set colorScheme variable for use in other settings
" Doesn't override preset scheme
" Background should always be set after colorscheme.
" Not sure if this still needs to be here if loaded before plugins...
if exists('&g:colors_name')
    let colorSch = g:colors_name
endif
" Only used if colorSch not set (plugins didn't get loaded)
let g:fallbackColorSch="default"

if !exists ('colorSch')
    if exists("$COLOURSCHEME") && $COLOURSCHEME != ""
        if !exists ('g:backgroundColour')
            if $COLOURSCHEME=~?"light"
                let g:backgroundColour="light"
            elseif $COLOURSCHEME=~?"dark"
                let g:backgroundColour="dark"
            endif
        endif
        let colorSch=substitute($COLOURSCHEME, '_dark', '', '')
        let colorSch=substitute(colorSch, '_light', '', '')
        let colorSch=substitute(colorSch, 'light', '', '')
        let colorSch=substitute(colorSch, 'dark', '', '')
        if $COLOURSCHEME=~?"onehalf"
          " Onehalf has two colourschemes, onehalflight and onehalfdark,
          " rather than using background.
          let colorSch=substitute($COLOURSCHEME, '-', '', '')
        endif
    else
        let colorSch=s:defaultColorSch
    endif
endif
" {]} Colours

if !exists ('g:backgroundColour')
    if g:hasGUI
        let g:backgroundColour=s:defaultBGGUI
    else
        " Default fallback for console bg colour
        let g:backgroundColour=s:defaultBGConsole
    endif
endif

exec 'set background=' . g:backgroundColour


"{[} GUI
" Guifont will be set to the first available font in this list.
" GTK fonts should be the top, since the gtk gvim doesn't use fallbacks.
let g:guiFonts = [
              \ "SauceCodePro Nerd Font",
              \ "SauceCodePro_NF",
              \ "SauceCodePro NF",
              \ "SauceCodeProNerdFontCo-Regular",
              \ "Source Code Pro Medium",
              \ "Source Code Pro",
              \ "Menlo",
              \ "Consolas"
              \ ]
if has("win32")
    let g:defaultFontSize = 11
elseif has("macunix")
    let g:defaultFontSize = 15
else
    let g:defaultFontSize = 11
endif

function! SetUpGUI()
    if g:ideMode
        set guioptions+=iagmrLtT
        " Full-screen gvim window
        " set lines=999 columns=999
    else
        " Remove menus to speed up startup
        set guioptions-=m
        set guioptions-=t
        " https://github.com/vim/vim/issues/5246
        au myVimrc VimEnter * :set guioptions-=T
        set guioptions+=M
        " Larger gvim window
        set lines=40 columns=120
    endif
    " Never use ugly tab page that overrides airline's
    set guioptions-=e
    " Don't use gui popups for simple questions, use console dialog (ensures
    " keyboard can always be used)
    set guioptions+=c
    " Use terminal window to execute commands
    " set guioptions+=!
    set guioptions+=p
    " Enables some basic mouse input
    set mouse=a
    set mousemodel="popup_setpos"
    " Default fallback for gui bg colour
    let g:termColors="24bit"
    if &guifont==?""
      " Call first, but it won't apply until called by the autocmd.
      " Call first to set it for parsing, eg to set useNF/usePF.
      call SetGFN()
      " au myVimrc GUIEnter * call SetGFN()
    endif
    if has("termguicolors")
        set termguicolors
    endif
endfunction
    "{]}

function! SetupNvimQT()
    if !exists(':GuiTabline')
        return
    endif
    GuiTabline 0
    GuiPopupmenu 0
    GuiScrollBar 1
    call SetGFN()
endfunction

function! SetUpNvimGUI(event)
    let g:hasGUI = 1
    call SetUpGUI()
    " Options provided by other GUIs that neovim might have.
    " if event['chan'] == 'nvim-qt'
    if exists('g:GuiLoaded')  " nvim-qt specific
        call SetupNvimQT()
    endif
    if exists('g:neovide')
    endif
    call SourcePluginFile('symbol_check.vim')
endfunction

if g:hasGUI
    " GUI is running or is about to start.
    if has('nvim')
        call SetUpGUI()
        au myVimrc UIEnter * call SetUpNvimGUI(v:event)
    else
        call SetUpGUI()
    endif
else
    "{[} Console
    set mouse=a
    " let &pastetoggle = '\e[201~'
    " noremap  <special> <expr> <Esc>[200~ <SID>XTermPasteBegin('0i')
    " inoremap <special> <expr> <Esc>[200~ <SID>XTermPasteBegin('')
    " cnoremap <special> <Esc>[200~ <nop>
    " cnoremap <special> <Esc>[201~ <nop>

    " {[} Colours
    " Use true colors
    if has("termguicolors") && exists("$COLORTERM") &&
                \ ($COLORTERM =~ "truecolor" || $COLORTERM =~ "24bit")
        set termguicolors
        " set Vim-specific sequences for RGB colors in tmux
        let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
        let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
        let g:termColors="24bit"
        set t_Co=256
    else
        if !exists("g:termColors") && exists("$COLORTERM")
            let g:termColors=$COLORTERM
        endif
        if $TERM =~ "-256color" && !exists("g:termColors")
            let g:termColors=256
            " This wasn't being set by default under TMUX!
            set t_Co=256
        else
            " 16 should be default, or settable.
            set t_Co=16
            let g:termColors=16
        endif
    endif


    " {]}
endif
"{]}

set showcmd
" Show cursor coords
set ruler
" Highligh search as you go
set incsearch
" Ignore case in searches excepted if an uppercase letter is used
set ignorecase smartcase
if has('nvim')
    set inccommand=split
    " when 'c'hanging, don't remove text. Put $ at end instead. Line will be
    " overwritten.
    " set cpoptions += "$"
endif

if has('nvim-0.4')
    set signcolumn=auto:4
endif
" Highlight search results. Use :noh to undo
" set hlsearch
" Show line numbers
set number
if v:version >= 703
    " Line numbers are displayed relative to current line.
    set relativenumber
endif
" Hide buffer (don't ask for save) when navigating away.
set hidden
set sessionoptions-=blank
set wildmenu
" Complete longest common string, list options. Then cycle each full match
set wildmode=list:longest,full
" Allows opening files in case insensitive way.
set wildignorecase
" Mapping usable in macros/maps to trigger completion menu.
set wildcharm=<tab>
" if exists('&pumblend') && &termguicolors && !g:liteMode
"     " Pseudo-transparency for popup menu.
"     set pumblend=20
"     set winblend=20
" endif
set scrolloff=5
set completeopt=longest,menu
try
  set completeopt+=popup
catch /E474:/
  set completeopt+=preview
endtry
if exists("g:ideMode") && g:ideMode == 1
    " Include tags and includes in completion.
    set complete+=i
    set complete+=t
    " set completeopt+=noselect
else
    set complete-=i     " Searching includes can be slow
endif
" When jumping to a fileline from current tab, don't change tabs.
set switchbuf=useopen,uselast
" Lower priority file suffixes in completion menus.
set suffixes+=.tmp,tags
set display=lastline
set diffopt+=vertical
set previewheight=6
" Close preview window.
autocmd myVimrc InsertLeave * pclose
function! s:previewWinSettings()
  if !&previewwindow
    return
  endif
  setlocal nobuflisted
  setlocal nonumber
  setlocal norelativenumber
endfunction
autocmd myVimrc BufWinEnter * if &previewwindow | call s:previewWinSettings() | endif


if v:version >= 703
    if has('nvim')
        " Note that viminfo is an alias for &shada in nvim.
        let s:viminfoName = 'nviminfo.shada'
    else
        let s:viminfoName = 'viminfo'
    endif
    let s:viminfoPath=PathExpand(g:vimfilesDir . '/' . s:viminfoName)
    " Need to use '/' as path sep, or '\\'. Not '/' as is created by
    " pathexpand.
    let s:viminfoPath=substitute(s:viminfoPath,'\\','/','g')
    exec 'set viminfo+=n' . s:viminfoPath
endif

" Extra slash at end means files will have unique names
" Backupdir possibly doesn't allow // on older versions
exec 'set backupdir=' . CreateVimDir("backup") . '//'
" au BufWritePre * let &bex = '-' . strftime("%Y%b%d%X") . '.vimbackup'
set backupext=.vimbackup
set backup
set writebackup
exec 'set directory=' . CreateVimDir("swap") . '//'
exec 'set viewdir=' . CreateVimDir("views") . '//'
if has('persistent_undo')
    if has('nvim')
        exec 'set undodir=' . CreateVimDir("nvimundo") . '//'
    else
        exec 'set undodir=' . CreateVimDir("undo") . '//'
    endif
    " Create undo file for inter-session undo
    set undofile
endif
set viewoptions-=options
" Superceded by light plugi 'vim-stay'.
" save folds
" autocmd myVimrc BufWinLeave *.* mkview
" autocmd myVimrc BufWinEnter *.* silent! loadview

" Autosave
" Automatically save before commands like :next and :make or when
" changing buffer.
set autowrite
function! Autosave()
    " Don't autosave if there is no buffer name, or readonly, and ensure
    " autowrite is set.
    if bufname('%') != '' && &ro != 1 && &modifiable == 1 && &autowrite
        " If no changes, don't touch modified timestamp.
        " Don't trigger autocmds.
        noa silent! update
    endif
endfunction
augroup autosave
    au!
    " Save on focus loss, leaving insert, leaving buffer.
    au autosave FocusLost,InsertLeave,BufLeave * call Autosave()
augroup end

" Checktime is used for things like autoread.
"https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044#383044
" on these events, any filename... and not in command mode then check files for changes
au myVimrc FocusLost,BufLeave * if mode() != 'c' | checktime | endif

set modeline
set modelines=5
" Important for security.
silent! set nomodelineexpr
set expandtab
set tabstop=4
let &shiftwidth=&tabstop
let &softtabstop=&shiftwidth
set smarttab
" formatoptins: See :h fo-table.
" Don't break long lines automatically.
set formatoptions +=l
" Continue comments
set formatoptions +=ro
if v:version >= 704
    " Remove comment leader on join.
    set formatoptions +=j
endif
set wrap
if v:version >= 800
    set listchars=tab:>-,trail:·,eol:¬,precedes:←,extends:→,nbsp:·
    autocmd myVimrc optionset wrap let &showbreak=''
    autocmd myVimrc optionset nowrap let &showbreak='→ '
    let &showbreak='→ '
    " put linebreak in number column? Doesn't seem to work...
    set cpoptions+=n
endif
" backspace and cursor keys wrap to previous/next line
set backspace=indent,eol,start whichwrap+=<,>,[,]
if v:version >= 703
    " Colour the 80th column
    set colorcolumn=80
endif
" Linebreak relates to when a line wraps (ie not in a word)
set linebreak
" Vertical window splits open on right side, horizontal below
set splitright
set splitbelow
set shortmess=a
" Don't seem to be set properly...
" https://en.wikipedia.org/wiki/Box-drawing_character#DOS
" set fillchars+="vert:│,stlnc:─"
set fillchars+="vert:║,stlnc:═,stl:\ ,"
set lazyredraw

function! MinimumUI()
  set laststatus=0
  set showtabline=0
  set showmode
  set nonumber norelativenumber foldcolumn=0
  set nolist nospell
  set colorcolumn=0
endfunction

function! SmallUIOnResize()
    if !exists('g:oldShortmess')
        let g:oldShortmess=&shortmess
    endif
    if &lines < 24
        " let &l:scrolloff=&lines/5
        set cmdheight=1
        " Can't afford to hard wrap by mistake.
        set textwidth=200
        set noshowcmd
        set shortmess=aWAFtI
        if &lines < 18
            " Hides airline/any other status bar.
            set laststatus=0
            set showtabline=0
            set noruler
            set noshowmode
            if has('nvim')
                set cmdheight=0
            else
                set cmdheight=1
            endif
        else
            set laststatus=1
            set showtabline=1
            set ruler
            set cmdheight=1
        endif
    else
        set cmdheight=2
        set laststatus=2
        set showtabline=2
        set showcmd
        set ruler
        let &shortmess=g:oldShortmess
        " setlocal scrolloff=-1  " return to global value
    endif
    if &columns < 15
        set nonumber
        set norelativenumber
    else
        set number
        set relativenumber
    endif
endfunction

if exists("g:minimumUI")
  call MinimumUI()
else
  " Add vimenter because resized doesn't trigger on startup, only on change.
  autocmd myVimrc VimEnter * call SmallUIOnResize()
  autocmd myVimrc VimResized * call SmallUIOnResize()
endif

" Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor = "latex"
if v:version >= 703
    set conceallevel=2
endif
set foldmethod=syntax
set foldminlines=6
" set foldmethod=marker
" set foldmarker={[},{]}
set foldnestmax=3
" Shows a column that displays folds.
" set foldcolumn=1
set foldminlines=3
" Most folds closed on bufopen, not all.
set foldlevelstart=1
set foldopen+=",insert"
" Use indent and manual folding
" au myVimrc BufReadPre * setlocal foldmethod=indent
" au myVimrc BufWinEnter * if &fdm == 'indent' | setlocal foldmethod=manual | endif

function! s:SetSpellFile()
  let b:spellfilename=PathExpand(expand('%:p:h') . '/custom-spellings-vim.'
        \ . &encoding . '.add')
  " Check if directory is writable. Can't check b:spellfilename directly on
  " windows because of backslashs.
  if filewritable(expand("%:p:h"))
    " Expand because of backslashes before spaces
    let &l:spellfile=expand(b:spellfilename)
  else
    setl spf=
  endif
endfunction
" set spellfile=$HOME/.vim-spell-en.utf-8.add
if exists('##OptionSet')
    au myVimrc OptionSet spell call <sid>SetSpellFile()
else
    au myVimrc BufEnter * call <sid>SetSpellFile()
endif
"Uses dictionary and source files to find matching words to complete.
"See help completion for source,
"Note: usual completion is on <C-n> but more trouble to press all the time.
"Never type the same word twice and maybe learn a new spellings!
"Use the Linux dictionary when spelling is in doubt.
"Window users can copy the file to their machine.
if has("unix") && !exists('&dictionary')
  set dictionary="/usr/dict/words"
endif

" No annoying sound on errors
set noerrorbells
" set novisualbell
" set t_vb=
" set tm=500

" default file explorer plugin.
let g:netrw_banner = 0
" Tree-style listing
let g:netrw_liststyle = 3
" Right split
let g:netrw_altv = 1
" Open files in vert split, use winsize % of netrw window for new file.
let g:netrw_browse_split = 2
let g:netrw_winsize = 80
" autocmd myVimrc BufRead * if isdirectory(@%) | exec 'Explore!' | endif

"highlight whitespace at the ends of lines
" autocmd InsertEnter * syn clear EOLWS | syn match EOLWS excludenl /\s\+\%#\@!$/
" autocmd InsertLeave * syn clear EOLWS | syn match EOLWS excludenl /\s\+$/
" highlight EOLWS ctermbg=red guibg=red

" Put buffer name in window title
function! s:SetTitle()
    if g:hasGUI
        " Exclude 'Vim' for guivim (because it'll have a logo)
        let l:app = ""
    elseif has("nvim")
        let l:app = "NVim"
    else
        if exists("g:noPlugin")
            let l:app = "Vi"
        else
            let l:app = "Vim"
        endif
    endif
    if g:ideMode == 1
        let l:preTitle = "|ide" . l:app . "| "
    elseif g:liteMode == 1
        let l:preTitle = "|lite" . l:app . "| "
    else
        if l:app == ""
            let l:preTitle = ""
        else
            let l:preTitle = "|" . l:app . "| "
        endif
    endif
    let l:filename = expand("%:t")
    if l:filename == ""
        let &titlestring = 'Vim - New Buffer'
    else
        let &titlestring = l:preTitle . l:filename . ' [' . expand("%:p:h") . ']'
    endif
    set title
endfunction

autocmd myVimrc BufEnter,Bufwrite * call <sid>SetTitle()

" Autoset new unnamed buffers to scratch, to get pencil stuff etc.
" No point doing it until we have entered some text.
augroup scratchSetStart
    au!
    autocmd TextChanged,InsertLeave {}
                \ setlocal ft=scratch |
                \ if &filetype == "" | setlocal ft=scratch | endif |
                \ au! scratchSetStart
augroup end
" Autoset new named buffers to scratch if no other specified
autocmd myVimrc BufNewFile * filetype detect | if &filetype == "" | setlocal ft=scratch | endif
" Pre-existing files without clear ft: use conf. Gives hash comments,
" highlights strings. Works for lots of small files.
autocmd myVimrc BufReadPost * filetype detect | if &filetype == ""  | setlocal ft=conf | endif
" Automatically detect the changed filetype on write. Currently only doing
" it if the previous buftype was scratch (ie unnamed, which in default vim
" would have done this anyway)
autocmd myVimrc BufWritePost * if &filetype == "scratch" | filetype detect | endif

autocmd myVimrc filetype scratch setlocal spell | setl ai

function! s:ReadTemplate()
    filetype detect
    let l:templateDir = PathExpand(s:scriptdir . '/templates')
    let l:templatePaths = split(globpath(l:templateDir, &filetype . '.*'), '\n')
    for templatePath in l:templatePaths
      exec 'silent! 0read ' . templatePath
    endfor
endfunction
autocmd myVimrc BufNewFile * call s:ReadTemplate()

" {[} Set cursor based on mode.
if !has('nvim')
    " block cursor in normal mode, line in other.
    if &term =~ "xterm\\|rxvt" || $TERM_PROGRAM =~ "mintty\\|xterm" || exists('$TMUX')
        let s:iCursor = "\<Esc>[5 q"
        let s:nCursor = "\<Esc>[1 q"
        let s:rCursor = "\<Esc>[3 q"
        let s:vCursor = "\<Esc>[2 q"
        " 1 or 0 -> blinking block
        " 2 solid block
        " 3 -> blinking underscore
        " 4 solid underscore
        " Recent versions of xterm (282 or above) also support
        " 5 -> blinking vertical bar
        " 6 -> solid vertical bar
    elseif $TERM_PROGRAM =~ "iTerm" || &term =~ "konsole"
        let s:iCursor = "\<Esc>]50;CursorShape=1\x7" " Vertical bar in insert mode
        let s:nCursor = "\<Esc>]50;CursorShape=0\x7" " Block in normal mode
        let s:rCursor = "\<Esc>]50;CursorShape=2\x7" " Underline in replace mode
        let s:vCursor = "\<Esc>]50;CursorShape=0\x7"
    endif
    " if $TERMTYPE =~ "mintty"
        " Requires mintty terminal.
        " let &t_ti.="\e[1 q" " termcap mode, for terminfo
        " let &t_SI.="\<Esc>[5 q"
        " let &t_EI.="\e[1 q"
        " let &t_te.="\e[0 q" " end termcap
        " let s:iCursor = "\e[5 q"
        " let s:nCursor = "\e[1 q"
        " let s:rCursor = "\e[1 q"
        " elseif $TERMTYPE ~= "xterm"

    if exists('s:iCursor')
        " if exists('$TMUX')
        "     let s:iCursor = "\<Esc>Ptmux;\<Esc>" . s:iCursor . "\<Esc>\\"
        "     let s:nCursor = "\<Esc>Ptmux;\<Esc>" . s:nCursor . "\<Esc>\\"
        "     let s:rCursor = "\<Esc>Ptmux;\<Esc>" . s:rCursor . "\<Esc>\\"
        "     let s:vCursor = "\<Esc>Ptmux;\<Esc>" . s:vCursor . "\<Esc>\\"
        " endif
        exec 'let &t_SI = "' . s:iCursor . '""'
        exec 'let &t_EI = "' . s:nCursor . '""'
        " exec 'let &t_VS = "' . s:vCursor . '""'
        if v:version >= 800
            exec 'let &t_SR = "' . s:rCursor . '""'
        endif
        " reset cursor when vim exits
        autocmd myVimrc VimLeave * silent !echo -ne "\033]112\007"
        " use \003]12;gray\007 for gnome-terminal and rxvt up to version 9.21
    endif
endif
" Cursor {]}

" Auto cd to working dir of this window's file
" autocmd BufEnter * silent! lcd %:p:h
" Cursorhold autocmds fire after 1s (default 4)
set updatetime=1000

" Close quickfix or location list if it's the last remaining window in the tab
" Disable status bar too
autocmd myVimrc WinEnter * if winnr('$') == 1 && &buftype == "quickfix" | quit | endif
autocmd myVimrc Filetype qf setlocal laststatus=0
autocmd myVimrc Filetype qf setlocal nonu
autocmd myVimrc Filetype qf setlocal norelativenumber
if v:version >= 703
    autocmd myVimrc Filetype qf setlocal colorcolumn=0
endif

if executable('rg')
	set grepformat=%f:%l:%m
	let &grepprg = 'rg --vimgrep' . (&smartcase ? ' --smart-case' : '')
elseif executable('ag')
	set grepformat=%f:%l:%m
	let &grepprg = 'ag --vimgrep' . (&smartcase ? ' --smart-case' : '')
endif

if has('patch-8.1.0360')
    set diffopt+=internal,algorithm:patience
      " set diffopt=indent-heuristic,algorithm:patience
endif

au myVimrc InsertLeave * set nopaste

if $TERM =~ 'kitty'
    let &t_ut=''
endif
if has('nvim-0.5')
  au TextYankPost * lua vim.highlight.on_yank {higroup="IncSearch", timeout=3000, on_visual=true}
endif
