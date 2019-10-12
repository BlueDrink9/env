" vim: foldmethod=marker
" vim: foldmarker={[},{]}

let s:scriptdir = fnamemodify(resolve(expand('<sfile>:p')), ':h')
augroup myIDE
    au!
augroup end
" {[} ---------- Misc ----------
" exec "Plug 'rhysd/vim-grammarous', { 'for': " . g:proseFileTypes . " }"
" Brilliant for projects with lots of similar files. Check out config
Plug 'https://github.com/tpope/vim-projectionist'
" Autoclose brackets, etc. Aims to mimic eclipse.
" Plug 'https://github.com/Townk/vim-autoclose'
" Autocomplete from other tmux panes' text
Plug 'https://github.com/wellle/tmux-complete.vim'

if v:version >= 703
    " visually show indentation
    Plug 'https://github.com/Yggdroot/indentLine'
    " Don't override my conceal settings.
    let g:indentLine_setConceal = 0
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
    call add(g:pluginSettingsToExec, 'nmap <silent> ]e <Plug>(ale_next_wrap)')
    call add(g:pluginSettingsToExec, 'nmap <silent> [e <Plug>(ale_previous_wrap)')
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

" {[} ---------- Completion ----------
" Awesome code completion, but requires specific installations
" Plug 'https://github.com/Valloric/YouCompleteMe'
if has("timers")

    exec 'source ' . g:plugindir . "/languageclient-neovim.vim"

    if has('node')
    exec 'source ' . g:plugindir . "/coc.nvim.vim"

    elseif has("python3")
    exec 'source ' . g:plugindir . "/deoplete.vim"

    else
        " Async completion engine, doesn't need extra installation.
        Plug 'maralla/completor.vim'
        " Use TAB to complete when typing words, else inserts TABs as usual.  Uses
        " dictionary, source files, and completor to find matching words to complete.

        " Note: usual completion is on <C-n> but more trouble to press all the time.
        " Never type the same word twice and maybe learn a new spellings!
        " Use the Linux dictionary when spelling is in doubt.
        function! Tab_Or_Completor() abort
            " If completor is already open the $(tab) cycles through suggested completions.
            if pumvisible()
                return "\<C-N>"
                " If completor is not open and we are in the middle of typing a word then
                " $(tab) opens completor menu.
            elseif col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
                return "\<C-R>=completor#do('complete')\<CR>"
            else
                " If we aren't typing a word and we press $(tab) simply do the normal $(tab)
                " action.
                return "\<Tab>"
            endif
        endfunction

        " Use $(tab) key to select completions.  Default is arrow keys.
        inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
        inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

        " Use tab to trigger auto completion.  Default suggests completions as you type.
        let g:completor_auto_trigger = 0
        inoremap <expr> <Tab> Tab_Or_Complete()
    endif
else
    Plug 'https://github.com/lifepillar/vim-mucomplete'
    let g:mucomplete#enable_auto_at_startup = 1
    " Only pause after no tyging for [updatetime]
    let g:mucomplete#delayed_completion = 1
    set completeopt+=menuone,noselect
endif

" Python completion, plus some refactor, goto def and usage features.
Plug 'https://github.com/davidhalter/jedi-vim', {'for' : 'python', 'do' : 'AsyncRun pip install jedi' }
let g:jedi#use_splits_not_buffers = "right"
" Using deoplete
if IsPluginUsed('deoplete.nvim')
    Plug 'deoplete-plugins/deoplete-jedi', {'for' : 'python', 'do' : 'AsyncRun pip install jedi' }
    let g:jedi#completions_enabled = 0
endif
let g:jedi#goto_command = "gpc"
let g:jedi#goto_assignments_command = "gpa"
let g:jedi#goto_definitions_command = "gpd"
let g:jedi#documentation_command = "K"
let g:jedi#usages_command = "gpu"
let g:jedi#completions_command = "<C-Space>"
let g:jedi#rename_command = "gpr"

" Intellisense engine for vim8 & neovim, full language server protocol support as VSCode.
" Uses VSCode-specific extensions, too. Seems to Just Work?
" Except do need to install all the sources. Check the readme.
" Installation is via command - annoying. Also uses hardcoded
" .json.
" npm installs extensions!
" if executable('node')
    " Plug 'https://github.com/neoclide/coc.nvim', {'branch': 'release'}
" endif

" {]} ---------- Completion----------

" {[} ---------- Tags ----------
Plug 'xolox/vim-misc'
if executable('ctags-exuberant') || executable('ctags')
    Plug 'ludovicchabant/vim-gutentags'
    Plug 'liuchengxu/vista.vim'
    " Stay in current window when opening vista.
    " let g:vista_stay_on_open = 0
    cabbrev tb Vista!!
    nnoremap <leader>t Vista
    " Uncomment to open tagbar automatically whenever possible
    autocmd myIDE BufEnter * nested :call tagbar#autoopen(0)
    if exists($USENF)
        let g:vista#renderer#enable_icon = 1
    endif
     let g:vista_executive_for = {
      \ 'markdown': 'toc',
      \ }
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
if (has("python") || has("python3")) && v:version >= 704
    Plug 'https://github.com/SirVer/ultisnips' " Snippit engine
    let g:UltiSnipsExpandTrigger="<c-e>"
    let g:UltiSnipsJumpForwardTrigger="<c-f>"
    let g:UltiSnipsJumpBackwardTrigger="<c-b>"
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

" {[} ---------- Syntax ----------
" {[} ------ Python ------
if has('python')
    Plug 'https://github.com/python-mode/python-mode', { 'branch': 'develop' }
    if has('python3')
        let g:pymode_python = 'python3'
    endif
    Plug 'https://github.com/tmhedberg/SimpylFold'
    let g:SimpylFold_docstring_preview = 1

endif
if v:version > 800 || has('nvim')
    " Plug 'szymonmaszke/vimpyter', {'do': 'pip install --user notedown'}
    let g:vimpyter_color=1
    if has('python3')
        " Plug 'jupyter-vim/jupyter-vim'
        let g:jupyter_mapkeys = 0
        function! JupyterMapKeys()
            " Run current file
            nnoremap <buffer> <silent> <localleader>a :JupyterRunFile<CR>
            " nnoremap <buffer> <silent> <localleader>I :JupyterImportThisFile<CR>
            " Change to directory of current file
            nnoremap <buffer> <silent> <localleader>cd :JupyterCd %:p:h<CR>
            " Send a selection of lines
            nnoremap <buffer> <silent> <localleader>b :JupyterSendCell<CR>
            nnoremap <buffer> <silent> <localleader>E :JupyterSendRange<CR>
            nmap     <buffer> <silent> <localleader>e <Plug>JupyterRunTextObj
            vmap     <buffer> <silent> <localleader>s <Plug>JupyterRunVisual
            nnoremap <buffer> <silent> <localleader>r :JupyterUpdateShell<CR>
            " Debugging maps
            " nnoremap <buffer> <silent> <localleader>b :PythonSetBreak<CR>
        endfunction
        au myIDE bufenter *.ipy call JupyterMapKeys()
    endif
endif
" {]} ------ Python ------

" {[} ---------- R ----------
if !has('nvim') && !has('job')
    " Unmaintained version that doesn't need vim 8
    Plug 'jcfaria/vim-r-plugin'
else
    Plug 'jalvesaq/Nvim-R'
    command! RStart :call StartR("R") | call <SID>SetnvimRShortcuts()
    function! s:SetnvimRShortcuts()
        " Rstop is already defined by plugin.
        command! -buffer RStartCustom :call StartR("custom")
        command! -buffer RRunFile :call SendFileToR("echo")
        command! -buffer RRunToHere :execute 'normal Vggo<Esc>' | :call SendSelectionToR("echo", "down")
        command! -buffer RRunSelection :call SendSelectionToR("echo", "stay")
        command! -buffer RRunChunk :call SendChunkToR("echo", "down")
        command! -buffer RRunMotion :set opfunc=SendMotionToR<CR>g@
        command! -buffer RRunParagraph :call SendParagraphToR("echo", "down")
        command! -buffer RRunLine :call SendLineToR("down")
        command! -buffer RObjects :call RObjBrowser()
        command! -buffer RClearObjects :call RClearAll()
        command! -buffer RHelpMappings :help Nvim-R-use
        nnoremap <buffer> <localleader>h :RRunToHere<CR>
        inoremap <buffer> <C-f> <C-O>:RRunLine<CR>
        nnoremap <buffer> <C-p> :RRunLine<CR>
    endfunction
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
Plug 'kovetskiy/sxhkd-vim', {'for': 'sxhkd' }

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
" {]} ---------- Syntax----------

" {[} ---------- Git ----------
if executable("git")
    " Advanced commit history browser
    Plug 'https://github.com/junegunn/gv.vim'
    " Better diff algs with :PatientDiff or :EnhancedDiff
    Plug 'https://github.com/chrisbra/vim-diff-enhanced'
endif
" {]} ---------- Git----------

" {[} ---------- IDE ----------
Plug 'https://github.com/janko/vim-test'
Plug 'https://github.com/mh21/errormarker.vim'
let &errorformat="%f:%l:%c: %t%*[^:]:%m,%f:%l: %t%*[^:]:%m," . &errorformat
let errormarker_disablemappings = 1
cabbrev er ErrorAtCursor
Plug 'ryanoasis/vim-devicons'
if has("python3")
    if has("nvim")
        Plug 'sakhnik/nvim-gdb', { 'do': ':!./install.sh \| UpdateRemotePlugins' }
    else
        " Vdebug for python, pyhp, perl, ruby
        Plug 'https://github.com/vim-vdebug/vdebug'
    endif
elseif has("python2")
    Plug 'https://github.com/vim-vdebug/vdebug', {'tag': 'v1.5.2'}
endif
" vebugger for gdb, lldb, jdb, pdb, rubydebug, node inspect.
Plug 'https://github.com/idanarye/vim-vebugger'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
" Look up documtenation for word under cursor with gk
Plug 'https://github.com/keith/investigate.vim'
" Quickly compile small files with :SCCompile
Plug 'https://github.com/xuhdev/SingleCompile'
" Customisable start screen, including MRU files
Plug 'https://github.com/mhinz/vim-startify'
let g:startify_session_dir = CreateVimDir(g:vimfilesDir . "/sessions/")
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
" {]} ---------- IDE----------

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
"     Plug 'Shougo/neosnippet.vim' " Snippet engine
"     Plug 'Shougo/neosnippet-snippets' " Snippets
"     Plug 'Shougo/vimproc.vim', { 'do': 'make' }
"     " Plug 'wellle/tmux-complete.vim' " Completion for tmux panes
"     " -> Neocomplete & Neocomplcache
"     " Use Tab and S-Tab to select candidate
"     inoremap <expr><Tab>  pumvisible() ? "\<C-N>" : "\<Tab>"
"     inoremap <expr><S-Tab> pumvisible() ? "\<C-P>" : "\<S-Tab>""
"     let g:neosnippet#enable_snipmate_compatibility = 1
"     " Use honza snippets instead of these defaults
"     let g:neosnippet#snippets_directory=CreateVimDir('Plugins/vim-snippets')
        " imap <expr><TAB> pumvisible() ? "\<C-n>" :
        "             \ neosnippet#expandable_or_jumpable() ?
        "             \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
        " imap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
        " inoremap <expr><CR> pumvisible() ? deoplete#mappings#close_popup() : "\<CR>"
" endif
" {]} ---------- Neosettings----------
