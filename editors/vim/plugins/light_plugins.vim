" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" {[} Litemode only / replacements.
if g:liteMode
    " Superlight airline (no plugins)
    " Plugin 'https://github.com/itchyny/lightline.vim'
    if !has('nvim')
        " Use better, but heavier, 'vim-sandwich' in main.
        Plugin 'https://github.com/tpope/vim-surround.git'
    endif
    " Lighter alt to airline for putting buffers in tabline.
    Plugin 'https://github.com/ap/vim-buftabline', {'event': ['BufNewFile', 'BufReadPost']}
endif
" {]} Litemode only

" {[}--- Misc ---
" Needed, really, because vim folding sucks otherwise.
Plugin 'Konfekt/FastFold'

" Allows plugin maps to use '.' to repeat
Plugin 'https://github.com/tpope/vim-repeat.git', {'keys': '.'}

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
if !has('nvim')
    Plugin 'https://github.com/tommcdo/vim-exchange'
endif
" Do replace, because cr is used for abolish. Yr is unused atm?
Plugin 'https://github.com/inkarkat/vim-ReplaceWithRegister', {'on': 
            \ ['<Plug>ReplaceWithRegisterOperator', '<Plug>ReplaceWithRegisterLine']}
" Plugin 'https://github.com/kana/vim-operator-user'
" Plugin 'https://github.com/kana/vim-operator-replace'

" Operator for start/end of text object. For example, d]i) deletes from the
" cursor to the end of the current parenthetical term
Plugin 'tommcdo/vim-ninja-feet',
            \ {'keys': MakeLazyKeys({"o": ["]", "[", "z]", "z["]})}
" {]}--- Operators ---

" {[}--- Visual changes ---
" Highlight f and t chars to get where you want.
if !has('nvim')
    Plugin 'unblevable/quick-scope'
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
