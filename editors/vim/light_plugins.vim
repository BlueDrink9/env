"--- Misc ---"
" Bunch of neat mappings, it's a tpope. Esp [n and ]n, for SCM conflict marks.
" And [<space> for addign newlines.
Plug 'https://github.com/tpope/vim-unimpaired'
Plug 'https://github.com/ntpeters/vim-better-whitespace'
Plug 'https://github.com/tmux-plugins/vim-tmux'
Plug 'https://github.com/christoomey/vim-tmux-navigator'
" Close buffers without changing window
Plug 'https://github.com/moll/vim-bbye'
Plug 'ericbn/vim-relativize'
" Highlight f and t chars to get where you want.
" TODO monitor progress of this branch. May be updated soon.
" Plug 'unblevable/quick-scope'
Plug 'https://github.com/bradford-smith94/quick-scope'
" Trigger a highlight in the appropriate direction when pressing these keys:
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
Plug 'https://github.com/altercation/vim-colors-solarized.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'https://github.com/maxbrunsfeld/vim-yankstack.git'
Plug 'https://github.com/jlanzarotta/bufexplorer.git'
" Replaced in favour of slightly heavier version tcomment.
" See https://github.com/wincent/wincent/commit/913e79724456976549244893e9025aa6fcf3cc1c
" Plug 'https://github.com/tpope/vim-commentary'
Plug 'https://github.com/kien/rainbow_parentheses.vim'
" Lighter alt to airline for putting buffers in tabline.
Plug 'https://github.com/ap/vim-buftabline'
" Superlight airline (no plugins)
" Plug 'https://github.com/itchyny/lightline.vim'

"--- Prose ---"
Plug 'xolox/vim-misc'
Plug 'https://github.com/xolox/vim-session'
Plug 'https://github.com/lervag/vimtex'
Plug 'https://github.com/reedes/vim-pencil'
Plug 'https://github.com/dkarter/bullets.vim'
