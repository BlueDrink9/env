" vim: foldmethod=marker
" vim: foldmarker={[},{]}

let s:scriptdir = fnamemodify(resolve(expand('<sfile>:p')), ':h')
augroup myIDE
    au!
augroup end

" {[} ---------- Misc ----------
if has('nvim-0.5')
    " Dependency for a lot of plugins
    Plugin 'nvim-lua/plenary.nvim'

    " For installing LSPs (and other packages)
    Plugin 'https://github.com/williamboman/mason.nvim'
    Plugin 'https://github.com/RubixDev/mason-update-all'
    Plugin 'https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim'
endif
if has('nvim-0.7')
    Plugin 'https://github.com/norcalli/nvim-colorizer.lua'
else
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
Plugin 'https://github.com/AndrewRadev/splitjoin.vim'
" Auto-add 'end' statements, eg endif.
" Has odd bug with prose fts.
" Plugin 'https://github.com/tpope/vim-endwise'
" ga on char shows all representations, not just dec oct hex.
Plug 'https://github.com/tpope/vim-characterize', {'on': '<Plug>(characterize)'}
" {]} ---------- Misc ----------

" {[} ---------- Visual ----------
" if has('nvim-0.8')
"     " Replaces message display
"     Plugin 'https://github.com/folke/noice.nvim'
"     Plugin 'rcarriga/nvim-notify'
"     Plugin 'MunifTanjim/nui.nvim'
" endif

" Call WhichKey to see mappings starting with a thing.
if has('nvim-0.5')
    " Also shows registers and marks on " and '/`
    Plugin 'https://github.com/folke/which-key.nvim'
else
    Plugin 'liuchengxu/vim-which-key'
    " Show registers in side window when you go to use them.
    Plugin 'junegunn/vim-peekaboo'
endif
" nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
if has('nvim-0.5')
    " Leader ? to get searchable (if using telescope) list of commands with
    " keybindings.
    Plugin 'sudormrfbin/cheatsheet.nvim', {'on': 'Cheatsheet'}
endif

if v:version >= 703
    " visually show indentation
    if has("nvim")
        Plugin 'https://github.com/lukas-reineke/indent-blankline.nvim'
    else
        Plugin 'https://github.com/Yggdroot/indentLine'
    endif
endif

" Not sure if I actually need this - review later!
" if has("patch-8.1-1880") && has('nvim')
"     " Gives behaviour like completeopt=popup for neovim.
"     Plugin 'https://github.com/ncm2/float-preview.nvim'
" endif

if has("nvim")
    Plugin 'winston0410/cmd-parser.nvim'
    Plugin 'https://github.com/winston0410/range-highlight.nvim'
endif
" Needs manual activation. :RainbowParen, :RainbowParen!
Plugin 'https://github.com/junegunn/rainbow_parentheses.vim', {'on': 'RainbowParen'}

" {]} ---------- Visual ----------

"{[} Searching and code info
if has('nvim')  " needs > 0.7
    Plugin 'nvim-telescope/telescope.nvim', { 'tag': '*' }
    silent! UnPlug 'ctrlp.vim'
    silent! UnPlug 'fzf.vim'
    Plugin 'https://github.com/nvim-lua/plenary.nvim'
    if IsCCompilerAvailable()
        if Executable("make")
            Plugin 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
        elseif Executable("cmake")
            Plugin 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build' }
        endif
    endif
    Plugin 'https://github.com/nvim-telescope/telescope-ui-select.nvim'
    Plugin 'LinArcX/telescope-changes.nvim'
    Plugin 'FeiyouG/command_center.nvim'
    Plugin 'https://github.com/debugloop/telescope-undo.nvim'

    Plugin 'fcying/telescope-ctags-outline.nvim'
    Plugin 'cljoly/telescope-repo.nvim'
endif

" Display the indentation context in a window above the code you are
" looking at (helps understand where you are in a long func/class).
Plugin 'wellle/context.vim'

"{]} Searching and code info

" {[} ---------- LSP ----------
" These would be unloaded for CoC.nvim, which does completion and LSP
" Deoplete and ale will use them though.
if has('nvim-0.5')
    Plugin 'https://github.com/neovim/nvim-lspconfig'
    Plugin 'https://github.com/williamboman/mason-lspconfig.nvim'
    " Nice visuals
    Plugin 'https://github.com/folke/trouble.nvim'
    " Create appropriate colours for old colourschemes
    Plugin 'https://github.com/folke/lsp-colors.nvim'
    Plugin 'https://github.com/Hrle97/nvim.diagnostic_virtual_text_config'
    Plugin 'https://github.com/kosayoda/nvim-lightbulb'
else
    if has('win32')
        Plugin 'autozimu/LanguageClient-neovim', {
                    \ 'branch': 'next',
                    \ 'do': 'powershell -executionpolicy bypass -File install.ps1',
                    \ }
    else
        Plugin 'autozimu/LanguageClient-neovim', {
                    \ 'branch': 'next',
                    \ 'do': 'bash install.sh',
                    \ }
    endif
endif
" {]} ---------- LSP ----------

" {[} ---------- Linting ----------
if has('nvim-0.5')
    " Haven't configured yet.
    " Plugin 'https://github.com/mfussenegger/nvim-lint'
    " Integrates linters with Nvim lsp
    " Plugin 'https://github.com/jose-elias-alvarez/null-ls.nvim'
    " Plugin 'https://github.com/jayp0521/mason-null-ls.nvim'
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
Plugin 'jeetsukumaran/vim-pythonsense'
if HasPython()
    Plugin 'https://github.com/python-mode/python-mode', { 'branch': 'develop' }
    Plugin 'https://github.com/tmhedberg/SimpylFold'
    " Python completion, plus some refactor, goto def and usage features.
    Plugin 'https://github.com/davidhalter/jedi-vim', {'for' : 'python', 'do' : 'pip install --user jedi' }
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
    else
        Plugin 'jalvesaq/Nvim-R'
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
Plugin 'SidOfc/mkdx'
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
    if has('nvim-0.5')
        Plugin 'https://github.com/TimUntersberger/neogit'
    else
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
Plugin 'https://github.com/keith/investigate.vim'
" Customisable start screen, including MRU files
Plugin 'https://github.com/mhinz/vim-startify'

" Prettifier. Can be passed a filetype with a bang and selection to just
" do that part of the file!
" Doesn't state any requirements in readme...
Plugin 'https://github.com/sbdchd/neoformat', {'on': 'Neoformat'}

if has('nvim-0.7')
lua << EOF
function check_treesitter_installable()
    -- ("tar" and "curl" or "git") and {
    local fn = vim.fn
    if fn.Executable("git") == 0 then
        if fn.Executable("curl") == 0 and fn.executable("tar") == 0 then
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
        Plugin 'https://github.com/nvim-treesitter/nvim-treesitter.git', {'do': ':TSUpdate'}
        Plugin 'nvim-treesitter/playground'

        Plugin 'https://github.com/ThePrimeagen/refactoring.nvim'
        Plugin 'https://github.com/danymat/neogen'
        Plugin 'https://github.com/RRethy/nvim-treesitter-endwise'
        Plugin 'nvim-treesitter/nvim-treesitter-context'
        UnPlug 'context.vim'
        Plugin 'nvim-treesitter/nvim-treesitter-textobjects'
        UnPlug 'vim-argumentative'
        UnPlug 'vim-pythonsense'
        UnPlug 'SimpylFold'
        Plugin 'https://github.com/JoosepAlviste/nvim-ts-context-commentstring'
        Plugin 'https://github.com/Wansmer/treesj'
        UnPlug 'splitjoin.vim'
        " folding enhancements
        Plugin 'kevinhwang91/promise-async'
        Plugin 'https://github.com/kevinhwang91/nvim-ufo'
        UnPlug('FastFold')
        Plugin 'https://gitlab.com/HiPhish/rainbow-delimiters.nvim'
        UnPlug('rainbow_parentheses.vim')
        " Structural search and replace.
        Plugin 'https://github.com/cshuaimin/ssr.nvim'
        command! SSR lua require("ssr").open()
        " lua vim.keymap.set({ "n", "x" }, "<leader>sr", function() require("ssr").open() end)

    endif
endif

" {]} ---------- IDE----------

" {[} ---------- Debugging ----------
" :UnstackFromClipboard to take a stack trace from the clipboard and open the
" relevant function calls in their own splits
Plugin 'https://github.com/mattboehm/vim-unstack', {'on': 'UnstackFromClipboard'}
if has('nvim-0.7')
    Plugin 'https://github.com/mfussenegger/nvim-dap'
    Plugin 'https://github.com/jayp0521/mason-nvim-dap.nvim'
    Plugin 'rcarriga/nvim-dap-ui'
    Plugin 'https://github.com/Weissle/persistent-breakpoints.nvim'
    Plugin 'https://github.com/ofirgall/goto-breakpoints.nvim'
    if IsPluginUsed("telescope.nvim")
        Plugin 'https://github.com/nvim-telescope/telescope-dap.nvim'
    endif
    if IsPluginUsed("nvim-treesitter")
        Plugin 'theHamsta/nvim-dap-virtual-text'
        Plugin 'mfussenegger/nvim-dap-python'
    endif
" Vimspector requires vim 8.1 with this patch.
elseif has("patch-8.1-1264") || has('nvim')
    " Just install all available plugins for now...
    Plugin 'https://github.com/puremourning/vimspector', { 'do': ':!./install_gadget.py --all --disable-tcl' }
    " Easier python debugging
    Plugin 'sagi-z/vimspectorpy', { 'do': { -> vimspectorpy#update() } }
    if IsPluginUsed("telescope.nvim")
        Plugin 'nvim-telescope/telescope-vimspector.nvim'
    endif
endif
" {]} ---------- Debugging ----------

" {[} ---------- Completion ----------

let s:fallback_completion = 1
if has("timers")
    let s:fallback_completion = 0
    if has('nvim-0.5')
        Plugin 'hrsh7th/cmp-nvim-lsp'
        Plugin 'hrsh7th/cmp-buffer'
        Plugin 'hrsh7th/cmp-path'
        Plugin 'hrsh7th/nvim-cmp'
        Plugin 'hrsh7th/cmp-vsnip'
        Plugin 'hrsh7th/cmp-nvim-lsp-signature-help'
        Plugin 'hrsh7th/cmp-nvim-lsp-document-symbol'
        Plugin 'https://github.com/f3fora/cmp-spell'
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

        if has("nvim")
            Plugin 'https://github.com/github/copilot.vim'
        endif

    elseif has("python3")
        if Executable("cmake")
            " Awesome code completion, but requires specific installations and
            " compiling a binary.
            call SourcePluginFile("ycm_install.vim")
        endif
        " Fallback to deoplete if YCM hasn't installed properly.
        if HasNvimPythonModule() && !exists("g:YCM_Installed")

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
Plugin 'https://github.com/honza/vim-snippets'
Plugin 'https://github.com/rafamadriz/friendly-snippets'
Plugin 'https://github.com/ericsia/vscode-python-snippet-pack-2.0', {'for': 'python'}
Plugin 'https://github.com/Antyos/vscode-openscad', {'for': 'openscad'}

if has('nvim-0.5') && !IsPluginUsed('coc.nvim')
    " Coc only support ultisips, neosnippet
    Plugin 'https://github.com/hrsh7th/vim-vsnip', {'on': [], 'event': ['InsertEnter']}
    Plugin 'hrsh7th/vim-vsnip-integ', {'on': [], 'event': ['InsertEnter']}
    Plugin 'octaltree/virtualsnip', { 'do': 'make', 'on': [], 'event': ['InsertEnter']}

elseif has('nvim') || v:version >= 740
    " Only requires 7.4, but recommends 8.
    Plugin 'Shougo/neosnippet.vim', {'on': [], 'event': ['InsertEnter']}
    Plugin 'Shougo/neosnippet-snippets'
elseif HasPython() && v:version >= 704
    Plugin 'https://github.com/SirVer/ultisnips' " Snippit engine
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

if has('nvim-0.5')
    Plugin 'https://github.com/hkupty/iron.nvim'
endif

" Useful for REPL, but can also send the commands back to the other window.
" Also dot repeatable.
" Have to specify direction to send, however.
" https://github.com/KKPMW/vim-sendtowindow
" Super lightweight, have to specify command for each filetype.
" Example config in readme.
" https://github.com/axvr/zepl.vim

" {]} ---------- REPL ----------
