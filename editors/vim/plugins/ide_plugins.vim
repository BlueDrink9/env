" vim: foldmethod=marker
" vim: foldmarker={[},{]}

augroup myIDE
    au!
augroup end

" {[} ---------- Misc ----------
if !has('nvim-0.7')
    " Highlight colors when used eg in css
    Plugin 'https://github.com/chrisbra/Colorizer'
endif

" Plugin 'rhysd/vim-grammarous', { 'for': g:proseFileTypes }
" Brilliant for projects with lots of similar files. Check out config
" Plugin 'https://github.com/tpope/vim-projectionist'
" Filetype can change within files.
" Autocomplete from other tmux panes' text
if $TMUX !=? ""
    Plugin 'https://github.com/wellle/tmux-complete.vim'
endif

" gS/gJ to split/join things onto separate/same lines.
if !has('nvim')
    Plugin 'https://github.com/AndrewRadev/splitjoin.vim'
endif
" Auto-add 'end' statements, eg endif.
" Has odd bug with prose fts.
" Plugin 'https://github.com/tpope/vim-endwise'
" ga on char shows all representations, not just dec oct hex.
Plug 'https://github.com/tpope/vim-characterize', {'on': '<Plug>(characterize)'}
" {]} ---------- Misc ----------

" {[} ---------- Visual ----------

" Call WhichKey to see mappings starting with a thing.
if !has('nvim-0.5')
    Plugin 'liuchengxu/vim-which-key'
    " Show registers in side window when you go to use them.
    Plugin 'junegunn/vim-peekaboo'
endif

" Needs manual activation. :RainbowParen, :RainbowParen!
if !has('nvim')
    Plugin 'https://github.com/junegunn/rainbow_parentheses.vim', {'on': 'RainbowParen'}
endif

" {]} ---------- Visual ----------

"{[} Searching and code info

" Display the indentation context in a window above the code you are
" looking at (helps understand where you are in a long func/class).
if !has('nvim')
    Plugin 'wellle/context.vim'
endif

"{]} Searching and code info

" {[} ---------- LSP ----------
if !has('nvim-0.5')
    if has('win32')
        let g:lcn_inst = 'powershell -executionpolicy bypass -File install.ps1'
    else
        let g:lcn_inst = 'bash install.sh'
    endif
    Plugin 'autozimu/LanguageClient-neovim', {
                \ 'branch': 'next',
                \ 'do': g:lcn_inst,
                \ }
endif
" {]} ---------- LSP ----------

" {[} ---------- Linting ----------
if has('nvim')
elseif has("timers")
    " Async linting
    Plugin 'https://github.com/dense-analysis/ale'
else
    " ----- syntastic -----
    Plugin 'https://github.com/vim-syntastic/syntastic.git'
endif
" {]} ---------- Linting----------

" {[} ---------- Tags ----------
" Plugin 'xolox/vim-misc'
" if executable('ctags-exuberant') || executable('ctags')
"     Plugin 'ludovicchabant/vim-gutentags'
"     " Plugin 'liuchengxu/vista.vim'
"     " Plugin 'majutsushi/tagbar'
" endif

" if executable('cscope') && !has('nvim')
"     " Automates the process of creating and connecting to database.
"     Plugin 'vim-scripts/cscope.vim'
" endif
" if has("nvim-0.5")
"     Plugin 'simrat39/symbols-outline.nvim'
" endif
" {]} ---------- Tags----------

" {[} ---------- Lang-specific ----------
" {[} ------ Python ------
" provides text objects and motions for Python classes, methods,
" functions, and doc strings
if !has('nvim')
    Plugin 'jeetsukumaran/vim-pythonsense', {'for': 'python'}
endif
if HasPython()
    if !has('nvim')
        Plugin 'https://github.com/python-mode/python-mode', { 'branch': 'develop', 'for': 'python' }
        Plugin 'https://github.com/tmhedberg/SimpylFold', {'for': 'python'}
    endif
    " Python completion, plus some refactor, goto def and usage features.
    if !has('nvim')
        Plugin 'https://github.com/davidhalter/jedi-vim', {'for' : 'python', 'do' : 'pip install --user jedi' }
    endif
    " Using deoplete
    if IsPluginUsed('deoplete.nvim')
        Plugin 'deoplete-plugins/deoplete-jedi', {'for' : 'python', 'do' : 'pip3 install --user jedi' }
    endif
endif

" Pip install jupytext. Converts notebooks to text format.
if Executable('jupytext')
    Plugin 'goerz/jupytext.vim'
endif

" {]} ------ Python ------

" {[} ---------- R ----------
if Executable('R')
    if !has('nvim') && !has('job')
        " Unmaintained version that doesn't need vim 8
        Plugin 'jcfaria/vim-r-plugin'
    endif
endif
" {]} ---------- R ----------

" {[} ------ C ------
Plugin 'octol/vim-cpp-enhanced-highlight', {'for': ['cpp'] }
Plugin 'https://github.com/WolfgangMehner/c-support', {'for': ['c', 'cpp'] }
" {]} ------ C ------

" May cause lag on scrolling.
Plugin 'tiagofumo/vim-nerdtree-syntax-highlight'
" Multi-lang support
let g:polyglot_disabled = ['latex', 'markdown', ]
Plugin 'https://github.com/sheerun/vim-polyglot'

" Advanced markdown formatting. Lots of features.
Plugin 'SidOfc/mkdx',  {'for': 'markdown'}
" Plugin 'vim-pandoc/vim-pandoc'
" Plugin 'vim-pandoc/vim-pandoc-syntax'

if has('python3')
    Plugin 'https://github.com/huleiak47/vim-AHKcomplete', {'for': 'autohotkey'}
endif
" {]} ---------- Lang-specific ----------

" {[} ---------- Git ----------
if Executable("git")
    " Advanced commit history browser
    Plugin 'https://github.com/junegunn/gv.vim', {'on': 'GV'}
    " Better diff algs with :PatientDiff or :EnhancedDiff
    if has("patch-8.1.0360")
        set diffopt+=internal,algorithm:patience
    else
        Plugin 'https://github.com/chrisbra/vim-diff-enhanced', {'on': ['PatientDiff', 'EnhancedDiff']}
    endif
    " View commit messages for current line in floating window.
    Plugin 'rhysd/git-messenger.vim', {'on': 'GitMessenger'}
    if !has('nvim-0.5')
        Plugin 'https://github.com/jreybert/vimagit'
    endif
endif
" {]} ---------- Git----------

" {[} ---------- IDE ----------
" for databases. Use with :Db (URL)
Plugin 'https://github.com/tpope/vim-dadbod', {'on': 'Db'}
Plugin 'https://github.com/kristijanhusak/vim-dadbod-completion', {'on': 'Db'}
Plugin 'https://github.com/kristijanhusak/vim-dadbod-ui', {'on': 'Db'}
Plugin 'https://github.com/janko/vim-test', {'on': [
            \ 'TestNearest',
            \ 'TestFile',
            \ 'TestSuite',
            \ 'TestLast',
            \ 'TestVisit'
            \ ]}
" Look up documtenation for word under cursor with gk
Plugin 'https://github.com/keith/investigate.vim', {'keys': 'gk'}
" Customisable start screen, including MRU files
Plugin 'https://github.com/mhinz/vim-startify'

" Prettifier. Can be passed a filetype with a bang and selection to just
" do that part of the file!
" Doesn't state any requirements in readme...
Plugin 'https://github.com/sbdchd/neoformat', {'on': 'Neoformat'}

" {]} ---------- IDE----------

" {[} ---------- Debugging ----------
" :UnstackFromClipboard to take a stack trace from the clipboard and open the
" relevant function calls in their own splits
Plugin 'https://github.com/mattboehm/vim-unstack', {'on': 'UnstackFromClipboard'}
if has("patch-8.1-1264") && !has('nvim')
    " Just install all available plugins for now...
    Plugin 'https://github.com/puremourning/vimspector', { 'do': ':!./install_gadget.py --all --disable-tcl' }
    " Easier python debugging
    Plugin 'sagi-z/vimspectorpy', { 'do': { -> vimspectorpy#update() } }
endif
" {]} ---------- Debugging ----------

" {[} ---------- Completion ----------

let s:fallback_completion = 1
if has("timers")
    let s:fallback_completion = 0
    if has('nvim-0.5')
        " use cmp
        " Super speedy, but slightly more complex requirements
        " https://github.com/ms-jpq/coq_nvim
    elseif has('node')
        " Intellisense engine for vim8 & neovim, full language server protocol support as VSCode.
        " Uses VSCode-specific extensions, too. Seems to Just Work?
        Plugin 'neoclide/coc.nvim', {'tag': '*', 'branch': 'release'}
        UnPlug 'dense-analysis/ale'
        UnPlug 'autozimu/LanguageClient-neovim'
        UnPlug 'neovim/nvim-lspconfig'
        UnPlug 'davidhalter/jedi-vim'
        UnPlug 'python-mode/python-mode'
        if IsPluginUsed("telescope.nvim")
            Plugin 'fannheyward/telescope-coc.nvim'
        endif
    elseif has("python3")
        if Executable("cmake")
            " Awesome code completion, but requires specific installations and
            " compiling a binary.
            call SourcePluginFile("ycm_install.vim")
        endif
        " Fallback to deoplete if YCM hasn't installed properly.
        if !exists("g:YCM_Installed")

            if exists("v:completed_item")
                " Shows function args from completion in cmd line.
                Plugin 'Shougo/echodoc.vim'
            endif
            call SourcePluginFile("deoplete_install.vim")
        elseif has("python")
            " Async completion engine, doesn't need extra installation.
            Plugin 'maralla/completor.vim'
        else
            let s:fallback_completion = 1
        endif
    else
        let s:fallback_completion = 1
    endif
endif
if s:fallback_completion == 1
    Plugin 'https://github.com/lifepillar/vim-mucomplete'
endif

" {]} ---------- Completion----------

" {[} ---------- Snippits ----------
" Snippet libs
Plugin 'https://github.com/honza/vim-snippets', {'on': [], 'event': ['InsertEnter']}
Plugin 'https://github.com/rafamadriz/friendly-snippets.git', {'on': [], 'event': ['InsertEnter']}
Plugin 'https://github.com/ericsia/vscode-python-snippet-pack-2.0', {'for': 'python'}
Plugin 'https://github.com/Antyos/vscode-openscad', {'for': 'openscad'}

if has('nvim')
elseif v:version >= 740
    " Only requires 7.4, but recommends 8.
    Plugin 'Shougo/neosnippet.vim', {'on': [], 'event': ['InsertEnter']}
    Plugin 'Shougo/neosnippet-snippets'
elseif HasPython() && v:version >= 704
    " Snippit engine
    Plugin 'https://github.com/SirVer/ultisnips'
else
    " {[} ---------- Snipmate ----------
    Plugin 'https://github.com/tomtom/tlib_vim.git'
    Plugin 'https://github.com/MarcWeber/vim-addon-mw-utils.git'
    Plugin 'https://github.com/garbas/vim-snipmate'
    " {]} ---------- Snipmate ----------
endif
" {]} ---------- Snippits----------

" {[} ---------- REPL ----------
" Not really working very well
" if has('nvim') && !has('win32')
"     Plugin 'https://github.com/michaelb/sniprun', {'do': 'bash install.sh'}
" endif

" Useful for REPL, but can also send the commands back to the other window.
" Also dot repeatable.
" Have to specify direction to send, however.
" https://github.com/KKPMW/vim-sendtowindow
" Super lightweight, have to specify command for each filetype.
" Example config in readme.
" https://github.com/axvr/zepl.vim

" {]} ---------- REPL ----------
