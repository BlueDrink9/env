"--- Misc ---"
" Bunch of neat mappings, it's a tpope. Esp [n and ]n, for SCM conflict marks.
" And [<space> for addign newlines.
Plug 'https://github.com/tpope/vim-unimpaired'
Plug 'nathanaelkane/vim-indent-guides'
" For switching between header and alt files
Plug 'vim-scripts/a.vim'
" Bsgrep for searching in all open buffers. Also Bsreplace, Bstoc.
Plug 'https://github.com/jeetsukumaran/vim-buffersaurus'
Plug 'https://github.com/ntpeters/vim-better-whitespace'
let g:show_spaces_that_precede_tabs=1
let g:better_whitespace_skip_empty_lines=1
let g:better_whitespace_operator='_s'
Plug 'https://github.com/tmux-plugins/vim-tmux'
Plug 'https://github.com/christoomey/vim-tmux-navigator'
" Close buffers without changing window
Plug 'https://github.com/moll/vim-bbye'
cabbrev bd Bdelete
Plug 'ericbn/vim-relativize'
if v:version >= 702
    " Highlight f and t chars to get where you want.
    " TODO monitor progress of this branch. May be updated soon.
    " Plug 'unblevable/quick-scope'
    Plug 'https://github.com/bradford-smith94/quick-scope'
    " Trigger a highlight in the appropriate direction when pressing these keys:
    let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
endif
Plug 'https://github.com/altercation/vim-colors-solarized.git'
if !has("gui_running")
    " if $TERM contains "-256color"
    " May be needed if terminal doesn't support.
    " TODO sort out base16 colours for solarized
    exec 'let g:' . colorSch . '_termcolors=256'
    exec 'let g:' . colorSch . '_base16 = 0'
    let base16colorspace = 256
    let g:solarized_termtrans = 1
endif
" Settings doesn't recommend this...
let g:solarized_contrast = "high"

Plug 'xolox/vim-misc'
" Map os commands (eg maximise), and open windows commands without shell
" popup.
Plug 'https://github.com/xolox/vim-shell'
if v:version >= 704
    Plug 'https://github.com/xolox/vim-session'
    let g:session_persist_globals = ['&spelllang', '&autoread', '&spell']
    let g:session_persist_colors = 0
    let g:session_persist_font = 0
    let g:session_default_to_last = 'yes'
    let g:session_autosave_periodic = 10
    let g:session_autosave = 'yes'
    let g:session_autoload = 'yes'
    let g:session_directory = CreateVimDir("vimfiles/sessions/")
    cabbrev cs CloseSession
    cabbrev os OpenSession
    cabbrev ss SaveSession
endif

Plug 'https://github.com/tpope/vim-surround.git'
Plug 'https://github.com/maxbrunsfeld/vim-yankstack.git'
if v:version >= 704
    Plug 'https://github.com/jlanzarotta/bufexplorer.git'
endif
" Replaced in favour of slightly heavier version tcomment.
" See https://github.com/wincent/wincent/commit/913e79724456976549244893e9025aa6fcf3cc1c
" Plug 'https://github.com/tpope/vim-commentary'
Plug 'https://github.com/junegunn/rainbow_parentheses.vim'
" Superlight airline (no plugins)
" Plug 'https://github.com/itchyny/lightline.vim'

"--- Prose ---"
Plug 'https://github.com/lervag/vimtex'
" Plug 'https://github.com/vim-latex/vim-latex'
" let g:Tex_DefaultTargetFormat="pdf"
if has('win32')
    let g:vimtex_view_general_viewer = 'sumatrapdf'
    let g:vimtex_view_general_options
                \ = '-reuse-instance -forward-search @tex @line @pdf'
    let g:vimtex_view_general_options_latexmk = '-reuse-instance'
endif
Plug 'https://github.com/reedes/vim-pencil'
Plug 'https://github.com/dkarter/bullets.vim'
Plug 'https://github.com/junegunn/goyo.vim'

"--- Syntax ---"
Plug 'octol/vim-cpp-enhanced-highlight'
let g:cpp_class_decl_highlight = 1
let g:cpp_member_variable_highlight = 1
Plug 'https://github.com/WolfgangMehner/c-support'

