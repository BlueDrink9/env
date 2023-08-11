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

" For switching between header and alt files
" Plug 'vim-scripts/a.vim'
" Close buffers without changing window
Plug 'https://github.com/moll/vim-bbye', {'on': 'Bdelete'}
cabbrev bd Bdelete
" {]} Misc

" {[}--- Yanks ---
" Provides access to clipboard via programs
" Note that this fork is basically Unmaintained, but there are so many others
" too that I don't know which to choose. None have all the PRs.
if !has('clipboard') || IsWSL()
    Plug 'https://github.com/kana/vim-fakeclip'
endif
if has('nvim')
    " Plug 'https://github.com/gbprod/yanky.nvim'
    Plug 'https://github.com/bfredl/nvim-miniyank'
elseif exists('##TextYankPost')
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
if has('nvim')
    Plug 'https://github.com/gbprod/substitute.nvim'
else
    Plug 'https://github.com/tommcdo/vim-exchange'
endif
" Do replace, because cr is used for abolish. Yr is unused atm?
Plug 'https://github.com/inkarkat/vim-ReplaceWithRegister'
" Plug 'https://github.com/kana/vim-operator-user'
" Plug 'https://github.com/kana/vim-operator-replace'

" Operator for start/end of text object. For example, d]i) deletes from the
" cursor to the end of the current parenthetical term
Plug 'tommcdo/vim-ninja-feet'
" {]}--- Operators ---

" {[}--- Visual changes ---
" Highlight f and t chars to get where you want.
if has('nvim')
    Plug 'https://github.com/jinh0/eyeliner.nvim'
elseif v:version >= 702
    Plug 'unblevable/quick-scope'
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
