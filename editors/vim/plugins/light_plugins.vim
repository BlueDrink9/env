" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" {[} Litemode only / replacements.
if g:liteMode
" Superlight airline (no plugins)
    " Plugin 'https://github.com/itchyny/lightline.vim'
    " {[} Replace Tcomment with commentary
    " Replaced in favour of slightly heavier version tcomment in main
    " plugins. See https://github.com/wincent/wincent/commit/913e79724456976549244893e9025aa6fcf3cc1c
    Plugin 'https://github.com/tpope/vim-commentary'
    " {]} Replace Tcomment with commentary
    " Use better 'vim-sandwich' in main.
    Plugin 'https://github.com/tpope/vim-surround.git'
    " Lighter alt to airline for putting buffers in tabline.
    Plugin 'https://github.com/ap/vim-buftabline'
endif
" {]} Litemode only

" {[}--- Misc ---
" Needed, really, because vim folding sucks otherwise.
Plugin 'https://github.com/Konfekt/FastFold'

" Allows plugin maps to use '.' to repeat
Plugin 'https://github.com/tpope/vim-repeat', {'keys': '.'}

" For switching between header and alt files
" Plugin 'vim-scripts/a.vim'
" {]} Misc

" {[}--- Yanks ---
" Provides access to clipboard via programs
" Note that this fork is basically Unmaintained, but there are so many others
" too that I don't know which to choose. None have all the PRs.
if !has('clipboard') || IsWSL()
    Plugin 'https://github.com/kana/vim-fakeclip'
endif
if has('nvim')
    " Plugin 'https://github.com/gbprod/yanky.nvim'
    Plugin 'https://github.com/bfredl/nvim-miniyank', {'keys': ['p', 'P']}
elseif exists('##TextYankPost')
    Plugin 'https://github.com/svermeulen/vim-yoink'
else
    Plugin 'https://github.com/maxbrunsfeld/vim-yankstack.git'
endif
" {]}--- Yanks ---

" {[}--- Operators ---
" Bunch of neat mappings, it's a tpope. Esp [n and ]n, for SCM conflict marks.
" And [<space> for addign newlines.
Plugin 'https://github.com/tpope/vim-unimpaired',
            \ {'keys': [']', '[', 'yo', '>', '<', '=']}
" cx to select an object, then cx again to swap it with first thing.
if has('nvim')
    Plugin 'gbprod/substitute.nvim', {'keys': ['cx']}
else
    Plugin 'https://github.com/tommcdo/vim-exchange'
endif
" Do replace, because cr is used for abolish. Yr is unused atm?
Plugin 'https://github.com/inkarkat/vim-ReplaceWithRegister'
" Plugin 'https://github.com/kana/vim-operator-user'
" Plugin 'https://github.com/kana/vim-operator-replace'

" Operator for start/end of text object. For example, d]i) deletes from the
" cursor to the end of the current parenthetical term
Plugin 'tommcdo/vim-ninja-feet'
" {]}--- Operators ---

" {[}--- Visual changes ---
" Highlight f and t chars to get where you want.
if has('nvim')
    Plugin 'https://github.com/jinh0/eyeliner.nvim', {'keys': ['f', 'F', 't', 'T']}
elseif v:version >= 702
    Plugin 'unblevable/quick-scope'
endif
if v:version >= 703
    Plugin 'https://github.com/ntpeters/vim-better-whitespace', {
                \ 'event': ['TextChanged', 'TextChangedI'],
                \ 'on': ['StripWhitespace', 'StripWhitespaceOnChangedLines']
                \ }
endif
" Distraction-free vim.
Plugin 'https://github.com/junegunn/goyo.vim', {'on' : ['Goyo',]}
" {]}--- Visual ---

" {[} --- TMUX ---
if $TMUX !=? ""
    Plugin 'https://github.com/tmux-plugins/vim-tmux'
    Plugin 'https://github.com/christoomey/vim-tmux-navigator'
    Plugin 'https://github.com/preservim/vimux'
endif
" {]} TMUX

" {[} ---------- Prose ----------
Plugin 'https://github.com/reedes/vim-pencil'
" {]} ---------- Prose----------
