" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" AI-powered completion and snippets for python. Very cool-looking.
" https://github.com/kiteco/vim-plugin
let s:scriptdir = fnamemodify(resolve(expand('<sfile>:p')), ':h')
augroup myIDE
    au!
augroup end

" {[} ---------- Misc ----------
if has('nvim-0.5')
    Plug 'https://github.com/nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/playground'
    " FZF alternative
    " Plug 'https://github.com/nvim-telescope/telescope.nvim'

endif
if has('nvim-0.5')
    " Show registers in floating window when you go to use them.
    Plug 'tversteeg/registers.nvim', { 'branch': 'main' }
else
    " Show registers in side window when you go to use them.
    Plug 'junegunn/vim-peekaboo'
    let g:peekaboo_delay = 200  " ms
endif
" Display the indentation context in a window above the code you are
" looking at (helps understand where you are in a long func/class).
Plug 'wellle/context.vim'
let g:context_filetype_blacklist = []
let g:context_add_autocmds = 1
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
" Call WhichKey to see mappings starting with a thing.
Plug 'liuchengxu/vim-which-key'
if has('nvim-0.5')
    Plug 'AckslD/nvim-whichkey-setup.lua'
endif
" nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>

if v:version >= 703
    " visually show indentation
    Plug 'https://github.com/Yggdroot/indentLine'
    let g:indentLine_enabled=1
    let g:indentLine_char="┆"
    " Don't override my conceal settings.
    let g:indentLine_setConceal = 0
    " let g:indentLine_setColors=0
" if v:version >= 702
    " Too hard to get working with terminal. Looks crap.
    " Plug 'nathanaelkane/vim-indent-guides'
    " let g:indent_guides_auto_colors = 1
    " let g:indent_guides_enable_on_vim_startup = 1
    " call add (g:customHLGroups, "clear IndentGuidesOdd")
    " " call add (g:customHLGroups, "IndentGuidesOdd ")
endif
if exists("v:completed_item")
    " Shows function args from completion in cmd line.
    Plug 'Shougo/echodoc.vim'
    let g:echodoc#enable_at_startup = 1
    let g:echodoc#type = 'signature'
endif
" Auto-add 'end' statements, eg endif.
" Has odd bug with prose fts.
" Plug 'https://github.com/tpope/vim-endwise'
" {]} ---------- Misc ----------

" {[} ---------- Linting ----------
" if v:version >= 800
if has("timers")
    " Async linting
    Plug 'https://github.com/w0rp/ale'
    let g:ale_sign_error = 'X'
    let g:ale_sign_warning = '!'
    let g:ale_max_signs = 50
    let g:ale_echo_delay = 50
    " Downside is having to restart vim if you install a new linter.
    let g:ale_cache_executable_check_failures = 1
    " let g:ale_open_list=1 " Auto-open error lsit
    autocmd myPlugins User pluginSettingsToExec nmap <silent> ]e <Plug>(ale_next_wrap)
    autocmd myPlugins User pluginSettingsToExec nmap <silent> [e <Plug>(ale_previous_wrap)
    " Unimpaired makes remapping tricky.
    let g:nremap = {"]e": "<Plug>(ale_next_wrap)","[e": "<Plug>(ale_previous_wrap)" }
    " Disabled in favour of LSP from LanguageClient-neovim.
    " let g:ale_linters = {'r': []}
else
    " ----- syntastic -----
    Plug 'https://github.com/vim-syntastic/syntastic.git'
    let g:syntastic_cpp_compiler_options="-std=c++11"
    "  Airline handles status stuff (or should)
    " set statusline+=%#warningmsg#
    " set statusline+=%{SyntasticStatuslineFlag()}
    " set statusline+=%*
    " let g:syntastic_stl_format = "[%E{Err: #%e L%fe}%B{, }%W{Warn: #%w L%fw}]"

    let g:syntastic_enable_signs=1
    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list = 1
    let g:syntastic_loc_list_height = 5
    " open errors when present, close when done.
    let g:syntastic_auto_loc_list = 1
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq = 0
    " let g:syntastic_python_checkers = ['pylint']
    let g:syntastic_vim_checkers = ['vint']

    let g:syntastic_error_symbol = 'X'
    let g:syntastic_warning_symbol = "!"
    au myIDE FileType tex let b:syntastic_mode = "passive"
    " TODO make this window-specific.
    " let g:syntastic_shell = "/bin/sh"
endif
" {]} ---------- Linting----------

" {[} ---------- Tags ----------
Plug 'xolox/vim-misc'
if executable('ctags-exuberant') || executable('ctags')
    Plug 'ludovicchabant/vim-gutentags'
    if !has('...')
        Plug 'liuchengxu/vista.vim'
        " Stay in current window when opening vista.
        " let g:vista_stay_on_open = 0
        let s:tagbarOpenCmd="Vista!!"
        if exists($USENF)
            let g:vista#renderer#enable_icon = 1
        endif
        let g:vista_executive_for = {
                    \ 'markdown': 'toc',
                    \ }
        let g:vista_fzf_preview = ['right:50%']
    else
        Plug 'majutsushi/tagbar'
        let s:tagbarOpenCmd="TagbarToggle"
        " Uncomment to open tagbar automatically whenever possible
        autocmd myIDE BufEnter * nested :call tagbar#autoopen(0)
    endif
    exec 'cabbrev tb ' . s:tagbarOpenCmd
    exec 'nnoremap <leader>tt :' . s:tagbarOpenCmd . '<cr>'
endif

if executable('cscope')
    " Automates the process of creating and connecting to database.
    Plug 'vim-scripts/cscope.vim'
    nnoremap <leader>if :call cscope#findInteractive(expand('<cword>'))<CR>
    " nnoremap <leader>l :call ToggleLocationList()<CR>
endif
" {]} ---------- Tags----------

" {[} ---------- Snippits ----------
Plug 'https://github.com/honza/vim-snippets' " Library of snippets
" Only requires 7.4, but recommends this.
if has("nvim") || v:version >= 800
    Plug 'Shougo/neosnippet.vim'
    Plug 'Shougo/neosnippet-snippets'
    let g:neosnippet#enable_snipmate_compatibility=1
    " let g:neosnippet#enable_conceal_markers=0
    call Imap(g:IDE_mappings.snippet_expand, "<Plug>(neosnippet_expand_or_jump)")
    call Vmap(g:IDE_mappings.snippet_expand, "<Plug>(neosnippet_expand_or_jump)")
    call Imap(g:IDE_mappings.snippet_next, "<Plug>(neosnippet_jump)")
    call Vmap(g:IDE_mappings.snippet_next, "<Plug>(neosnippet_jump)")
    " imap <expr><TAB>
    "             \ pumvisible() ? "\<C-n>" :
    "             \ neosnippet#expandable_or_jumpable() ?
    "             \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
    " smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
    "             \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
elseif HasPython() && v:version >= 704
    Plug 'https://github.com/SirVer/ultisnips' " Snippit engine
    let g:UltiSnipsExpandTrigger = g:IDE_mappings.snippet_expand
    let g:UltiSnipsJumpForwardTrigger = g:IDE_mappings.snippet_next
    let g:UltiSnipsJumpBackwardTrigger = g:IDE_mappings.snippet_prev
    " Disable autotrigger
    " au myIDE VimEnter * au! UltiSnips_AutoTrigger
    " augroup ultisnips
    "     au!
    "     " Load default/all snippets
    "     autocmd BufEnter * UltiSnipsAddFiletypes alltypes
    " augroup end
    " Maybe use these to map <CR> to trigger in future?
    " autocmd! User UltiSnipsEnterFirstSnippet
    " autocmd User UltiSnipsEnterFirstSnippet call CustomInnerKeyMapper()
    " autocmd! User UltiSnipsExitLastSnippet
    " autocmd User UltiSnipsExitLastSnippet call CustomInnerKeyUnmapper()
    "
else
    " {[} ---------- Snipmate ----------
    Plug 'https://github.com/tomtom/tlib_vim.git'
    Plug 'https://github.com/MarcWeber/vim-addon-mw-utils.git'
    Plug 'https://github.com/garbas/vim-snipmate'
    let g:snipMate = {}
    let g:snipMate['description_in_completion'] = 1
    let g:snipMate['no_match_completion_feedkeys_chars'] = ''
    " Load default/all snippets
    autocmd myIDE BufEnter * SnipMateLoadScope alltypes
    imap <C-F> <Plug>snipMateNextOrTrigger
    smap <C-F> <Plug>snipMateNextOrTrigger
    imap <C-E> <Plug>snipMateTrigger
    smap <C-E> <Plug>snipMateTrigger
    imap <C-B> <Plug>snipMateBack
    smap <C-B> <Plug>snipMateBack
    " {]} ---------- Snipmate ----------
endif
" way smaller engine than ultisnips, not really much func. Can't use snip libs.
" Plug 'https://github.com/joereynolds/vim-minisnip'
" {]} ---------- Snippits----------

" {[} ---------- Lang-specific ----------
" {[} ------ Python ------
" provides text objects and motions for Python classes, methods,
" functions, and doc strings
Plug 'jeetsukumaran/vim-pythonsense'
if HasPython()
    Plug 'https://github.com/python-mode/python-mode', { 'branch': 'develop' }
    let g:pymode_options_max_line_length = 0
    let g:pymode_rope = 1
    let g:pymode_lint_message = 1
    let g:pymode_lint_on_write = 1
    let g:pymode_lint_unmodified = 1
    let g:pymode_lint = 1
    let g:pymode_rope_rename_bind = g:IDE_mappings.rename
    let g:pymode_rope_rename_module_bind = g:IDE_mappings.renameModule

    if has('python3')
        " if has('nvim')
        "     " semantic highlighting, including scope-based.
        "     " Doesn't seemd to be working atm, disabling highlighting for all
        "     " buffers.
        "     Plug 'numirias/semshi', {'do': ':UpdateRemotePlugins'}
        " endif
    endif
    Plug 'https://github.com/tmhedberg/SimpylFold'
    let g:SimpylFold_docstring_preview = 1

    " Python completion, plus some refactor, goto def and usage features.
    Plug 'https://github.com/davidhalter/jedi-vim', {'for' : 'python', 'do' : 'pip install --user jedi' }
    " Using deoplete
    if IsPluginUsed('deoplete.nvim')
        Plug 'deoplete-plugins/deoplete-jedi', {'for' : 'python', 'do' : 'pip3 install --user jedi' }
        let g:jedi#completions_enabled = 0
    endif
    let g:jedi#use_splits_not_buffers = "right"
    let g:jedi#goto_command = g:IDE_mappings.definition
    let g:jedi#goto_assignments_command = g:IDE_mappings.implementation
    let g:jedi#goto_definitions_command = g:IDE_mappings.definition
    let g:jedi#documentation_command = g:IDE_mappings.documentation
    let g:jedi#usages_command = g:IDE_mappings.references
    let g:jedi#completions_command = "Tab"
    let g:jedi#rename_command = g:IDE_mappings.rename
    Plug 'https://github.com/Vimjas/vim-python-pep8-indent'
endif

" Pip install jupytext. Converts notebooks to text format.
if executable('jupytext')
    Plug 'goerz/jupytext.vim'
    let g:jupytext_fmt = 'py:percent'
    function! s:jupytextSetup()
        " Use py:percent folding instead of SimpylFold python folding.
        let b:loaded_SimpylFold = 1
        " Override normal autosave with nested one, so it triggers update instead
        " of erasing buffer.
        au! myVimrc FocusLost,InsertLeave,BufLeave *
        au myVimrc FocusLost,InsertLeave,BufLeave * ++nested call Autosave()
        setlocal foldmethod=expr
    endfunction
    " Bufread does't work because the plugin overrides bufreadcmd?
    autocmd myIDE filetype jupytext ++nested call s:jupytextSetup()
endif

" {]} ------ Python ------

" {[} ---------- R ----------
if !has('nvim') && !has('job')
    " Unmaintained version that doesn't need vim 8
    Plug 'jcfaria/vim-r-plugin'
else
    call SourcePluginFile("nvim-R.vim")
endif
" R output is highlighted with current colorscheme
let g:rout_follow_colorscheme = 1
" Always split horizontally.
let R_rconsole_width = 0
let R_rconsole_height = 15

" let R_min_editor_width = 99
" R commands in R output are highlighted
let g:Rout_more_colors = 1
let R_esc_term = 0
let R_assign = 3
let R_latex_build_dir = 'latexbuild'
" This seems to no longer be permitted, despite still being in the
" docs...
" let R_openhtml = 2 " Reload, or open if not.
let R_openhtml = 1 " Always open
let g:markdown_fenced_languages = ['r', 'python']
let g:rmd_fenced_languages = ['r', 'python']
" {[} Mappings
" let R_user_maps_only = 1
" {]} Mappings
" Requires ncm2
" Plug 'https://github.com/gaalcaras/ncm-R'
" {]} ---------- R ----------

" {[} ------ C ------
Plug 'octol/vim-cpp-enhanced-highlight'
let g:cpp_class_decl_highlight = 1
let g:cpp_member_variable_highlight = 1
Plug 'https://github.com/WolfgangMehner/c-support', {'for': ['c', 'cpp'] }
let g:C_Ctrl_j = 'off'
Plug 'https://github.com/dragfire/Improved-Syntax-Highlighting-Vim'
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
Plug 'https://github.com/sheerun/vim-polyglot'
let g:polyglot_disabled = ['latex', 'markdown' ]

" Advanced markdown formatting. Lots of features.
Plug 'SidOfc/mkdx'
let g:mkdx#settings = {
      \ 'enter':          { 'shift': 1 },
      \ 'map':            { 'prefix': '<localleader>', 'enable': 1 },
      \ 'toc':            { 'text': 'Table of Contents', 'update_on_write': 1 },
      \ 'fragment':       { 'complete': 0 },
      \ 'highlight':      { 'enable': 1 },
      \ 'fold':           { 'enable': 1 },
      \ 'auto_update':    { 'enable': 1 }
    \ }
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
    let g:git_messenger_no_default_mappings=v:true
    " Include diff for all files in commit. Could be 'current'
    let g:git_messenger_include_diff="all"
    " Move cursor into popup for easier scrolling. Can manually do it by
    " running command a second time.
    let g:git_messenger_always_into_popup=v:true
endif
" {]} ---------- Git----------

" {[} ---------- IDE ----------
Plug 'https://github.com/janko/vim-test'
Plug 'https://github.com/mh21/errormarker.vim'
let &errorformat="%f:%l:%c: %t%*[^:]:%m,%f:%l: %t%*[^:]:%m," . &errorformat
let errormarker_disablemappings = 1
cabbrev er ErrorAtCursor
Plug 'ryanoasis/vim-devicons'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
" Look up documtenation for word under cursor with gk
Plug 'https://github.com/keith/investigate.vim'
" Quickly compile small files with :SCCompile
Plug 'https://github.com/xuhdev/SingleCompile'
" Customisable start screen, including MRU files
Plug 'https://github.com/mhinz/vim-startify'
let g:startify_session_dir = CreateVimDir("sessions")
let g:startify_lists = [
            \ { 'type': 'sessions',  'header': ['   Sessions']       },
            \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
            \ { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
            \ { 'type': 'files',     'header': ['   MRU']            },
            \ { 'type': 'commands',  'header': ['   Commands']       },
            \ ]
" let g:startify_bookmarks = [ {'c': '~/.vimrc'}, '~/.zshrc' ]
let g:startify_session_sort = 1
let g:startify_custom_indices = ['a','r','s','t','f','p','d','h','l','o','v','c','m']
let g:startify_custom_indices += ['A','R','S','T','F','P','D','H','L','O','V','C','M']
let g:startify_custom_header = 'startify#fortune#boxed()'

" Highlight colors when used eg in css
Plug 'https://github.com/chrisbra/Colorizer'

" Test running
Plug 'janko-m/vim-test'
" make test commands execute using dispatch.vim
let test#strategy = "dispatch"

" Prettifier. Can be passed a filetype with a bang and selection to just
" do that part of the file!
" Doesn't state any requirements in readme...
Plug 'https://github.com/sbdchd/neoformat'
nnoremap g= :Neoformat<CR>
" :Minimap to create a special buffer that gives an outline of the current
" file, synced to where you scroll. Uses drawille library (braille unicode)
Plug 'https://github.com/severin-lemaignan/vim-minimap'

if has("patch-8.1-1880") && has('nvim')
    " Gives behaviour like completeopt=popup for neovim.
    Plug 'https://github.com/ncm2/float-preview.nvim'
    let g:float_preview#docked = 0
endif

" {]} ---------- IDE----------

" {[} ---------- Debugging ----------
" if has('nvim-0.5')
" Plug 'https://github.com/Pocco81/DAPInstall.nvim'
" Plug 'https://github.com/mfussenegger/nvim-dap'
    " endif
" :UnstackFromClipboard to take a stack trace from the clipboard and open the
" relevant function calls in their own splits
Plug 'https://github.com/mattboehm/vim-unstack'
" IDK what mapping to use here.
let g:unstack_mapkey="<F2>"
" Top to bottom splits
let g:unstack_layout = "portrait"
" Vimspector requires vim 8.1 with this patch.
if has("patch-8.1-1264") || has('nvim')
    call SourcePluginFile("vimspector.vim")
else
    if has("python3")
        if has("nvim")
            Plug 'sakhnik/nvim-gdb', { 'do': ':!./install.sh \| UpdateRemotePlugins' }
        else
            " Vdebug for python, pyhp, perl, ruby
            silent! Plug 'https://github.com/vim-vdebug/vdebug'
        endif
    elseif has("python2")
        Plug 'https://github.com/vim-vdebug/vdebug', {'tag': 'v1.5.2'}
    endif
    " vebugger for gdb, lldb, jdb, pdb, rubydebug, node inspect.
    " plug 'https://github.com/idanarye/vim-vebugger'
endif
" {]} ---------- Debugging ----------

" {[} ---------- Completion ----------

" Enable completion from keywords in syntax files via omnifunc.
" Theoretically a built-in plugin, but doesn't seem to load...
Plug 'https://github.com/vim-scripts/SyntaxComplete'
autocmd myPlugins Filetype *
            \	if &omnifunc == "" |
            \		setlocal omnifunc=syntaxcomplete#Complete |
            \	endif
" This function gives list of completion items, for use in other
" plugins.
" let s:syntaxKeywords = OmniSyntaxList( [] )

if has("timers")

    if has('nvim-0.5')
        " This will be unloaded for CoC.nvim
        call SourcePluginFile("nvim-lspconfig.lua")
        call SourcePluginFile("nvim-compe.lua")
    else
        " This will be unloaded for CoC.nvim
        call SourcePluginFile("languageclient-neovim.vim")
    endif
    if has('node')
        " Intellisense engine for vim8 & neovim, full language server protocol support as VSCode.
        " Uses VSCode-specific extensions, too. Seems to Just Work?
        call SourcePluginFile("coc.nvim.vim")
    elseif has("python3")
        if executable("cmake")
            " Awesome code completion, but requires specific installations and
            " compiling a binary.
            call SourcePluginFile("ycm.vim")
        endif
        " Fallback to deoplete if YCM hasn't installed properly.
        if HasNvimPythonModule() && !exists("g:YCM_Installed")
            call SourcePluginFile("deoplete.vim")
        endif

    elseif has("python")
        " Async completion engine, doesn't need extra installation.
        Plug 'maralla/completor.vim'
        " Use TAB to complete when typing words, else inserts TABs as usual.  Uses
        " dictionary, source files, and completor to find matching words to complete.

        " Check the plugin has loaded correctly before overriding
        " completion command.
        function! s:completorSetCompletionCommand()
            if exists('completor#do')
                let g:completionCommand = "\<C-R>=completor#do('complete')\<CR>"
            endif
        endfunc
        autocmd myPlugins User pluginSettingsToExec call s:completorSetCompletionCommand()
        let g:completor_auto_trigger = 1

    endif
elseif has("python3") && executable("cmake")
    " Awesome code completion, but requires specific installations and
    " compiling a binary.
    call SourcePluginFile("ycm.vim")
else
    Plug 'https://github.com/lifepillar/vim-mucomplete'
    let g:mucomplete#enable_auto_at_startup = 1
    " Only pause after no typing for [updatetime]
    let g:mucomplete#delayed_completion = 1
    set completeopt+=menuone,noselect
endif

" {]} ---------- Completion----------

" {[} ---------- Neosettings ----------
" " https://github.com/kepbod/ivim/blob/master/vimrc
" if has('lua') && v:version > 703
"     Plug 'Shougo/neocomplete.vim' " Auto completion framework
"     let g:neocomplete#enable_at_startup=1
"     let g:neocomplete#data_directory=CreateVimDir('neocache')
"     let g:neocomplete#enable_auto_delimiter=1
"     " Use <C-E> to close popup
"     inoremap <expr><C-E> neocomplete#cancel_popup()
"     inoremap <expr><CR> delimitMate#WithinEmptyPair() ?
"                 \ "\<C-R>=delimitMate#ExpandReturn()\<CR>" :
"                 \ pumvisible() ? neocomplete#close_popup() : "\<CR>""
"     if !exists('g:neocomplete#force_omni_input_patterns')
"         let g:neocomplete#force_omni_input_patterns={}
"     endif
"     let g:neocomplete#force_omni_input_patterns.python=
"                 \ '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
" else
"     Plug 'Shougo/vimproc.vim', { 'do': 'make' }
"     " Use honza snippets instead of these defaults
"     let g:neosnippet#snippets_directory=CreateVimDir('Plugins/vim-snippets')
        " imap <expr><TAB> pumvisible() ? "\<C-n>" :
        "             \ neosnippet#expandable_or_jumpable() ?
        "             \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
        " inoremap <expr><CR> pumvisible() ? deoplete#mappings#close_popup() : "\<CR>"
" endif
" {]} ---------- Neosettings----------
