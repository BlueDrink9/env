" vim: foldmethod=marker
" vim: foldmarker={[},{]}

let s:scriptdir = fnamemodify(resolve(expand('<sfile>:p')), ':h')
augroup myIDE
    au!
augroup end

" {[} ---------- Misc ----------
if has('nvim-0.5')
    " Dependency for a lot of plugins
    Plug 'nvim-lua/plenary.nvim'

    " For installing LSPs (and other packages)
    Plug 'https://github.com/williamboman/mason.nvim'
    Plug 'https://github.com/RubixDev/mason-update-all'
    Plug 'https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim'
endif
if has('nvim-0.7')
    Plug 'https://github.com/norcalli/nvim-colorizer.lua'
endif

" Plug 'rhysd/vim-grammarous', { 'for': g:proseFileTypes }
" Brilliant for projects with lots of similar files. Check out config
Plug 'https://github.com/tpope/vim-projectionist'
" Filetype can change within files.
Plug 'Shougo/context_filetype.vim'
" Autoclose brackets, etc. Aims to mimic eclipse. I don't need to use it,
" mapping to auto-add brackets on enter is all I need.
" Plug 'https://github.com/Townk/vim-autoclose'
" Autocomplete from other tmux panes' text
Plug 'https://github.com/wellle/tmux-complete.vim'

" gS/gJ to split/join things onto separate/same lines.
Plug 'https://github.com/AndrewRadev/splitjoin.vim'

if has("nvim")
    Plug 'https://github.com/winston0410/cmd-parser.nvim'
endif

if exists("v:completed_item")
    " Shows function args from completion in cmd line.
    Plug 'Shougo/echodoc.vim'
endif
" Auto-add 'end' statements, eg endif.
" Has odd bug with prose fts.
" Plug 'https://github.com/tpope/vim-endwise'
" ga on char shows all representations, not just dec oct hex.
Plug 'https://github.com/tpope/vim-characterize'
" {]} ---------- Misc ----------

" {[} ---------- Visual ----------
" if has('nvim-0.8')
"     " Replaces message display
"     Plug 'https://github.com/folke/noice.nvim'
"     Plug 'rcarriga/nvim-notify'
"     Plug 'MunifTanjim/nui.nvim'
" endif

" Call WhichKey to see mappings starting with a thing.
if has('nvim-0.5')
    " Also shows registers and marks on " and '/`
    Plug 'https://github.com/folke/which-key.nvim'
    UnPlug 'registers.nvim'
else
    Plug 'liuchengxu/vim-which-key'
    " Show registers in side window when you go to use them.
    Plug 'junegunn/vim-peekaboo'
endif
" nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
if has('nvim-0.5')
    " Leader ? to get searchable (if using telescope) list of commands with
    " keybindings.
    Plug 'sudormrfbin/cheatsheet.nvim', {'on': 'Cheatsheet'}
    Plug 'nvim-lua/popup.nvim'
endif

if v:version >= 703
    " visually show indentation
    if has("nvim")
        Plug 'https://github.com/lukas-reineke/indent-blankline.nvim'
    else
        Plug 'https://github.com/Yggdroot/indentLine'
    endif
endif

if has("patch-8.1-1880") && has('nvim')
    " Gives behaviour like completeopt=popup for neovim.
    Plug 'https://github.com/ncm2/float-preview.nvim'
endif

if has("nvim")
    Plug 'https://github.com/winston0410/range-highlight.nvim'
endif
" Needs manual activation. :RainbowParen, :RainbowParen!
Plug 'https://github.com/junegunn/rainbow_parentheses.vim'

" {]} ---------- Visual ----------

"{[} Searching and code info
if IsPluginUsed("telescope.nvim")
    Plug 'fcying/telescope-ctags-outline.nvim'
    Plug 'cljoly/telescope-repo.nvim'
endif

" Display the indentation context in a window above the code you are
" looking at (helps understand where you are in a long func/class).
Plug 'wellle/context.vim'

"{]} Searching and code info

" {[} ---------- LSP ----------
" These would be unloaded for CoC.nvim, which does completion and LSP
" Deoplete and ale will use them though.
if has('nvim-0.5')
    Plug 'https://github.com/neovim/nvim-lspconfig'
    Plug 'https://github.com/williamboman/mason-lspconfig.nvim'
    " Nice visuals
    Plug 'https://github.com/folke/trouble.nvim'
    " Create appropriate colours for old colourschemes
    Plug 'https://github.com/folke/lsp-colors.nvim'
    Plug 'https://github.com/Hrle97/nvim.diagnostic_virtual_text_config'
    Plug 'https://github.com/kosayoda/nvim-lightbulb'
    Plug 'https://github.com/nanotee/sqls.nvim'
else
    if has('win32')
        Plug 'autozimu/LanguageClient-neovim', {
                    \ 'branch': 'next',
                    \ 'do': 'powershell -executionpolicy bypass -File install.ps1',
                    \ }
    else
        Plug 'autozimu/LanguageClient-neovim', {
                    \ 'branch': 'next',
                    \ 'do': 'bash install.sh',
                    \ }
    endif
endif
" {]} ---------- LSP ----------

" {[} ---------- Linting ----------
" if v:version >= 800
if has('nvim-0.5')
    " Haven't configured yet.
    " Plug 'https://github.com/mfussenegger/nvim-lint'
    " Integrates linters with Nvim lsp
    " Plug 'https://github.com/jose-elias-alvarez/null-ls.nvim'
    Plug 'https://github.com/jayp0521/mason-null-ls.nvim'
elseif has("timers")
    " Async linting
    Plug 'https://github.com/dense-analysis/ale'
else
    " ----- syntastic -----
    Plug 'https://github.com/vim-syntastic/syntastic.git'
endif
" {]} ---------- Linting----------

" {[} ---------- Tags ----------
Plug 'xolox/vim-misc'
if executable('ctags-exuberant') || executable('ctags')
    Plug 'ludovicchabant/vim-gutentags'
    Plug 'liuchengxu/vista.vim'
    " Plug 'majutsushi/tagbar'
endif

if executable('cscope')
    " Automates the process of creating and connecting to database.
    Plug 'vim-scripts/cscope.vim'
endif
if has("nvim-0.5")
    Plug 'simrat39/symbols-outline.nvim'
endif
" {]} ---------- Tags----------

" {[} ---------- Lang-specific ----------
" {[} ------ Python ------
" provides text objects and motions for Python classes, methods,
" functions, and doc strings
Plug 'jeetsukumaran/vim-pythonsense'
if HasPython()
    Plug 'https://github.com/Vimjas/vim-python-pep8-indent'
    Plug 'https://github.com/python-mode/python-mode', { 'branch': 'develop' }


    if has('python3')
        " if has('nvim')
        "     " semantic highlighting, including scope-based.
        "     " Doesn't seemd to be working atm, disabling highlighting for all
        "     " buffers.
        "     Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}
        " endif
    endif
    Plug 'https://github.com/tmhedberg/SimpylFold'

    " Python completion, plus some refactor, goto def and usage features.
    Plug 'https://github.com/davidhalter/jedi-vim', {'for' : 'python', 'do' : 'pip install --user jedi' }
    " Using deoplete
    if IsPluginUsed('deoplete.nvim')
        Plug 'deoplete-plugins/deoplete-jedi', {'for' : 'python', 'do' : 'pip3 install --user jedi' }
    endif
endif

" Pip install jupytext. Converts notebooks to text format.
if executable('jupytext')
    Plug 'goerz/jupytext.vim'
endif

" {]} ------ Python ------

" {[} ---------- R ----------
if executable('R')
    if !has('nvim') && !has('job')
        " Unmaintained version that doesn't need vim 8
        Plug 'jcfaria/vim-r-plugin'
    else
        Plug 'jalvesaq/Nvim-R'
    endif
endif
" {]} ---------- R ----------

" {[} ------ C ------
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'https://github.com/WolfgangMehner/c-support', {'for': ['c', 'cpp'] }
" Plug 'https://github.com/dragfire/Improved-Syntax-Highlighting-Vim'
" For extensive cpp IDE stuff.
" a.vim incompat with replacement provided here.

" Plug 'https://github.com/LucHermitte/lh-dev'
" Plug 'https://github.com/LucHermitte/mu-template'
" Plug 'tomtom/stakeholders_vim.git'
" Plug 'https://github.com/LucHermitte/lh-brackets' " Ooooh boy this one's problematic.
" Plug 'https://github.com/LucHermitte/lh-vim-lib'
" Plug 'luchermitte/lh-cpp'
" {]} ------ C ------

" May cause lag on scrolling.
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
" Multi-lang support
let g:polyglot_disabled = ['latex', 'markdown', ]
Plug 'https://github.com/sheerun/vim-polyglot'


" Advanced markdown formatting. Lots of features.
Plug 'SidOfc/mkdx'
" Plug 'vim-pandoc/vim-pandoc'
" Plug 'vim-pandoc/vim-pandoc-syntax'

" Syntax highlight ranges with a different filetype to the rest of the doc.
Plug 'https://github.com/inkarkat/vim-ingo-library'
Plug 'https://github.com/inkarkat/vim-SyntaxRange'

if has('python3')
    Plug 'https://github.com/huleiak47/vim-AHKcomplete', {'for': 'autohotkey'}
endif
" {]} ---------- Lang-specific ----------

" {[} ---------- Git ----------
if executable("git")
    " Advanced commit history browser
    Plug 'https://github.com/junegunn/gv.vim'
    " Better diff algs with :PatientDiff or :EnhancedDiff
    Plug 'https://github.com/chrisbra/vim-diff-enhanced'
    " View commit messages for current line in floating window.
    " :GitMessenger
    Plug 'rhysd/git-messenger.vim'
    if has('nvim-0.5')
        Plug 'https://github.com/TimUntersberger/neogit'
    else
        Plug 'https://github.com/jreybert/vimagit'
    endif
endif
" {]} ---------- Git----------

" {[} ---------- IDE ----------
" for databases. Use with :Db (URL)
Plug 'https://github.com/tpope/vim-dadbod'
Plug 'https://github.com/kristijanhusak/vim-dadbod-completion'
Plug 'https://github.com/kristijanhusak/vim-dadbod-ui'
if v:version < 708
    Plug 'https://github.com/janko/vim-test'
    Plug 'https://github.com/mh21/errormarker.vim'
endif
Plug 'ryanoasis/vim-devicons'
" Look up documtenation for word under cursor with gk
Plug 'https://github.com/keith/investigate.vim'
" Quickly compile small files with :SCCompile
Plug 'https://github.com/xuhdev/SingleCompile'
" Customisable start screen, including MRU files
Plug 'https://github.com/mhinz/vim-startify'

" Highlight colors when used eg in css
Plug 'https://github.com/chrisbra/Colorizer'

" Test running
Plug 'janko-m/vim-test'

" Prettifier. Can be passed a filetype with a bang and selection to just
" do that part of the file!
" Doesn't state any requirements in readme...
Plug 'https://github.com/sbdchd/neoformat'

if has('nvim-0.7')
lua << EOF
function check_treesitter_installable()
    -- ("tar" and "curl" or "git") and {
    local fn = vim.fn
    if fn.executable("git") == 0 then
        if fn.executable("curl") == 0 and fn.executable("tar") == 0 then
            return false
        end
    end
    if not fn.IsCCompilerAvailable() then
        return false
    end
    return true
end
EOF

    if luaeval("check_treesitter_installable()")
        Plug 'https://github.com/nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
        Plug 'nvim-treesitter/playground'

        Plug 'https://github.com/ThePrimeagen/refactoring.nvim'
        Plug 'https://github.com/danymat/neogen'
        Plug 'https://github.com/RRethy/nvim-treesitter-endwise'
        Plug 'nvim-treesitter/nvim-treesitter-context'
        UnPlug 'context.vim'
        Plug 'nvim-treesitter/nvim-treesitter-textobjects'
        UnPlug 'vim-argumentative'
        Plug 'https://github.com/JoosepAlviste/nvim-ts-context-commentstring'
        " Two treesitter splitjoin alternatives. In development, poor docs.
        " Plug 'https://github.com/Wansmer/treesj'
        " Plug 'https://github.com/aarondiel/spread.nvim'
        " UnPlug 'splitjoin.vim'
        " folding enhancements
        Plug 'kevinhwang91/promise-async'
        Plug 'https://github.com/kevinhwang91/nvim-ufo'
        UnPlug('FastFold')
        " Plug 'https://github.com/mrjones2014/nvim-ts-rainbow'
        " UnPlug('rainbow_parentheses.vim')
    endif
endif

" {]} ---------- IDE----------

" {[} ---------- Debugging ----------
" :UnstackFromClipboard to take a stack trace from the clipboard and open the
" relevant function calls in their own splits
Plug 'https://github.com/mattboehm/vim-unstack'
if has('nvim-0.7')
    Plug 'https://github.com/mfussenegger/nvim-dap'
    Plug 'https://github.com/jayp0521/mason-nvim-dap.nvim'
    Plug 'rcarriga/nvim-dap-ui'
    Plug 'https://github.com/Weissle/persistent-breakpoints.nvim'
    Plug 'https://github.com/ofirgall/goto-breakpoints.nvim'
    if IsPluginUsed("telescope.nvim")
        Plug 'https://github.com/nvim-telescope/telescope-dap.nvim'
    endif
    if IsPluginUsed("nvim-treesitter")
        Plug 'theHamsta/nvim-dap-virtual-text'
        Plug 'mfussenegger/nvim-dap-python'
    endif
" Vimspector requires vim 8.1 with this patch.
elseif has("patch-8.1-1264") || has('nvim')
    " Just install all available plugins for now...
    Plug 'https://github.com/puremourning/vimspector', { 'do': ':!./install_gadget.py --all --disable-tcl' }
    " Easier python debugging
    Plug 'sagi-z/vimspectorpy', { 'do': { -> vimspectorpy#update() } }
    if IsPluginUsed("telescope.nvim")
        Plug 'nvim-telescope/telescope-vimspector.nvim'
    endif
endif
" {]} ---------- Debugging ----------

" {[} ---------- Completion ----------

" Enable completion from keywords in syntax files via omnifunc.
" Theoretically a built-in plugin, but doesn't seem to load...
Plug 'https://github.com/vim-scripts/SyntaxComplete'

let s:fallback_completion = 1
if has("timers")
    let s:fallback_completion = 0
    if has('node')
        " Intellisense engine for vim8 & neovim, full language server protocol support as VSCode.
        " Uses VSCode-specific extensions, too. Seems to Just Work?
        Plug 'neoclide/coc.nvim', {'tag': '*', 'branch': 'release'}
        let g:ale_enabled = 0
        let g:LanguageClient_autoStart = 0
        UnPlug 'autozimu/LanguageClient-neovim'
        UnPlug 'neovim/nvim-lspconfig'
        UnPlug 'davidhalter/jedi-vim'
        UnPlug 'python-mode/python-mode'
        if IsPluginUsed("telescope.nvim")
            Plug 'fannheyward/telescope-coc.nvim'
        endif

        if has("nvim")
            Plug 'https://github.com/github/copilot.vim'
        endif

    elseif has('nvim-0.5')
        Plug 'hrsh7th/cmp-nvim-lsp'
        Plug 'hrsh7th/cmp-buffer'
        Plug 'hrsh7th/cmp-path'
        Plug 'hrsh7th/nvim-cmp'
        Plug 'hrsh7th/cmp-vsnip'
        Plug 'hrsh7th/cmp-nvim-lsp-signature-help'
        Plug 'hrsh7th/cmp-nvim-lsp-document-symbol'
        Plug 'https://github.com/f3fora/cmp-spell'
        " Super speedy, but slightly more complex requirements
        " https://github.com/ms-jpq/coq_nvim


    elseif has("python3")
        if executable("cmake")
            " Awesome code completion, but requires specific installations and
            " compiling a binary.
            call SourcePluginFile("ycm_install.vim")
        endif
        " Fallback to deoplete if YCM hasn't installed properly.
        if HasNvimPythonModule() && !exists("g:YCM_Installed")
            call SourcePluginFile("deoplete_install.vim")
        elseif has("python")
            " Async completion engine, doesn't need extra installation.
            Plug 'maralla/completor.vim'
        else
            let s:fallback_completion = 1
        endif
    else
        let s:fallback_completion = 1
    endif
endif
if s:fallback_completion == 1
    Plug 'https://github.com/lifepillar/vim-mucomplete'
endif

" {]} ---------- Completion----------

" {[} ---------- Snippits ----------
" Snippet libs
Plug 'https://github.com/honza/vim-snippets'
Plug 'https://github.com/rafamadriz/friendly-snippets'
Plug 'https://github.com/ericsia/vscode-python-snippet-pack-2.0', {'for': 'python'}
Plug 'https://github.com/Antyos/vscode-openscad', {'for': 'openscad'}

if has('nvim-0.5') && !IsPluginUsed('coc.nvim')
    " Coc only support ultisips, neosnippet
" if has('nvim') || v:version >= 800

    Plug 'https://github.com/hrsh7th/vim-vsnip', {'on': [] }
    Plug 'hrsh7th/vim-vsnip-integ', {'on': [] }
    call LoadPluginOnInsertEnter('vim-vsnip')
    call LoadPluginOnInsertEnter('vim-vsnip-integ')
    Plug 'octaltree/virtualsnip', { 'do': 'make', 'on': [] }
    call LoadPluginOnInsertEnter('virtualsnip')

elseif has('nvim') || v:version >= 740
    " Only requires 7.4, but recommends 8.
    Plug 'Shougo/neosnippet.vim', {'on': [] }
    call LoadPluginOnInsertEnter('neosnippet.vim')
    Plug 'Shougo/neosnippet-snippets'
elseif HasPython() && v:version >= 704
    Plug 'https://github.com/SirVer/ultisnips' " Snippit engine
else
    " {[} ---------- Snipmate ----------
    Plug 'https://github.com/tomtom/tlib_vim.git'
    Plug 'https://github.com/MarcWeber/vim-addon-mw-utils.git'
    Plug 'https://github.com/garbas/vim-snipmate'
    " {]} ---------- Snipmate ----------
endif
" way smaller engine than ultisnips, not really much func. Can't use snip libs.
" Plug 'https://github.com/joereynolds/vim-minisnip'
" {]} ---------- Snippits----------

" {[} ---------- REPL ----------
" Not really working very well
" if has('nvim') && !has('win32')
"     Plug 'https://github.com/michaelb/sniprun', {'do': 'bash install.sh'}
" endif

if has('nvim-0.5')
    Plug 'https://github.com/hkupty/iron.nvim'
endif

" Useful for REPL, but can also send the commands back to the other window.
" Also dot repeatable.
" Have to specify direction to send, however.
" https://github.com/KKPMW/vim-sendtowindow
" Super lightweight, have to specify command for each filetype.
" Example config in readme.
" https://github.com/axvr/zepl.vim

" {]} ---------- REPL ----------
