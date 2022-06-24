" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" {[} Litemode only / replacements.
if g:liteMode
" Superlight airline (no plugins)
    " Plug 'https://github.com/itchyny/lightline.vim'
    " {[} Replace Tcomment with commentary
    " Replaced in favour of slightly heavier version tcomment in main
    " plugins. See https://github.com/wincent/wincent/commit/913e79724456976549244893e9025aa6fcf3cc1c
    Plug 'https://github.com/tpope/vim-commentary'
    " {]} Replace Tcomment with commentary
    " Use better 'vim-sandwich' in main.
    Plug 'https://github.com/tpope/vim-surround.git'
    " Lighter alt to airline for putting buffers in tabline.
    Plug 'https://github.com/ap/vim-buftabline'
endif
" {]} Litemode only

" {[}--- Misc ---
" Needed, really, because vim folding sucks otherwise.
Plug 'https://github.com/Konfekt/FastFold'

" Allows plugin maps to use '.' to repeat
Plug 'https://github.com/tpope/vim-repeat'

" Jump to specified place in file with file(L:C)
Plug 'https://github.com/wsdjeg/vim-fetch'
" For switching between header and alt files
" Plug 'vim-scripts/a.vim'
" if v:version >= 800 || has("patch-7.4.1829")
if has("timers")
    " Commands sent to shell with AsyncRun appear in qf window.
    " use AsyncRun! to prevent autoscroll.
    Plug 'https://github.com/skywind3000/asyncrun.vim'
    let g:hasAsyncrun = 1
else
    let g:hasAsyncrun = 0
endif
" Confirms opening empty file on tabcomplete
Plug 'https://github.com/EinfachToll/DidYouMean'
" Close buffers without changing window
Plug 'https://github.com/moll/vim-bbye', {'on': 'Bdelete'}
cabbrev bd Bdelete

Plug 'https://github.com/chrisbra/csv.vim', {'for': 'csv'}
" {]} Misc

" {[}--- Yanks ---
" Provides access to clipboard via programs
" Note that this fork is basically Unmaintained, but there are so many others
" too that I don't know which to choose. None have all the PRs.
if !has('clipboard') || IsWSL()
    Plug 'https://github.com/kana/vim-fakeclip'
endif
" Needs unite/denite, no mappings by default.
" Maybe later on, put in ide and don't load yankring if idemode.
" if exists('##TextYankPost')
"     Plug 'Shougo/neoyank.vim'
"     let g:neoyank#file = &directory . 'yankring.txt'
" nmap <leader>p :unite history/yank
" else
if exists('##TextYankPost')
    Plug 'https://github.com/svermeulen/vim-yoink'
else
    Plug 'https://github.com/maxbrunsfeld/vim-yankstack.git'
endif
" {]}--- Yanks ---

" {[}--- Operators ---
" Bunch of neat mappings, it's a tpope. Esp [n and ]n, for SCM conflict marks.
" And [<space> for addign newlines.
Plug 'https://github.com/tpope/vim-unimpaired'
" cx to select an object, then cx again to swap it with first thing.
Plug 'https://github.com/tommcdo/vim-exchange'
" Do replace, because cr is used for abolish. Yr is unused atm?
Plug 'https://github.com/inkarkat/vim-ReplaceWithRegister'
" Operator for start/end of text object. For example, d]i) deletes from the
" cursor to the end of the current parenthetical term
Plug 'tommcdo/vim-ninja-feet'
" {]}--- Operators ---

" {[}--- Visual changes ---
if v:version >= 702
    " Highlight f and t chars to get where you want.
    Plug 'unblevable/quick-scope'
    " Plug 'https://github.com/bradford-smith94/quick-scope'
endif
if v:version >= 703
    Plug 'https://github.com/ntpeters/vim-better-whitespace'
endif
" Distraction-free vim.
Plug 'https://github.com/junegunn/goyo.vim', {'on' : ['Goyo',]}
" {]}--- Visual ---

" {[} --- TMUX ---
if executable('tmux')
    Plug 'https://github.com/tmux-plugins/vim-tmux'
    Plug 'https://github.com/christoomey/vim-tmux-navigator'
    Plug 'https://github.com/preservim/vimux'
endif
" {]} TMUX

" {[} ---------- Prose ----------
Plug 'https://github.com/reedes/vim-pencil'
" {]} ---------- Prose----------
