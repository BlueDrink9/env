" vim: foldmethod=marker
" vim: foldmarker={[},{]}

let g:plugin_config_dir = fnamemodify(resolve(expand('<sfile>:p')), ':h')
augroup myIDE
    au!
augroup end

" {[} ---------- Misc ----------
"
" {]} ---------- Misc ----------

if IsPluginUsed('vim-peekaboo')
    let g:peekaboo_delay = 200  " ms
endif

if IsPluginUsed('context.vim')
    let g:context_filetype_blacklist = []
    let g:context_add_autocmds = 1
endif

if IsPluginUsed('indentLine')
    let g:indentLine_enabled=1
    let g:indentLine_char="┆"
    " Don't override my conceal settings.
    let g:indentLine_setConceal = 0
    " let g:indentLine_setColors=0
endif
if IsPluginUsed('echodoc.vim')
    let g:echodoc#enable_at_startup = 1
    let g:echodoc#type = 'signature'
endif

if IsPluginUsed('vim-which-key')
    nnoremap <leader> <cmd>WhichKey '<leader>'<CR>
    vnoremap <leader> <cmd>WhichKeyVisual '<leader>'<CR>
endif

" {]} ---------- Visual ----------


" {[} ---------- LSP ----------
if !has('nvim')
    if IsPluginUsed('languageclient-neovim')
        call SourcePluginFile("languageclient-neovim.vim")
    endif
endif
" {]} ---------- LSP ----------

" {[} ---------- Linting ----------

if IsPluginUsed('ale')
    let g:ale_sign_error = 'X'
    let g:ale_sign_warning = '!'
    let g:ale_change_sign_column_color = 1
    let g:ale_max_signs = 50
    let g:ale_echo_delay = 200
    " Faster but have to restart vim if you install a new linter.
    let g:ale_cache_executable_check_failures = 1
    if Executable('nice')
        " Don't run linters at the expense of general system
        let g:ale_command_wrapper = 'nice -n5 %*'
    endif
    " let g:ale_open_list=1 " Auto-open error lsit
    nmap <silent> ]e <Plug>(ale_next_wrap)
    nmap <silent> [e <Plug>(ale_previous_wrap)
    " Unimpaired makes remapping tricky.
    let g:nremap = {"]e": "<Plug>(ale_next_wrap)","[e": "<Plug>(ale_previous_wrap)" }
    " Disabled in favour of LSP from LanguageClient-neovim.
    " let g:ale_linters = {'r': []}
endif

if IsPluginUsed('syntastic.git')
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
if IsPluginUsed('vista.vim')
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
endif
if IsPluginUsed('tagbar')
    let s:tagbarOpenCmd="TagbarToggle"
    " Uncomment to open tagbar automatically whenever possible
    " autocmd myIDE BufEnter * nested :call tagbar#autoopen(0)
endif
if exists("s:tagbarOpenCmd")
    exec 'cabbrev tb ' . s:tagbarOpenCmd
    exec 'nnoremap <leader>tt <cmd>' . s:tagbarOpenCmd . '<cr>'
endif
" {]} ---------- Tags----------

" {[} ---------- Lang-specific ----------
" {[} ------ Python ------
if IsPluginUsed('python-mode')
    let g:pymode_options = 0
    let g:pymode_options_max_line_length = 88
    " let g:pymode_rope = 1
    let g:pymode_lint = 1
    let g:pymode_lint_message = 1
    let g:pymode_lint_on_write = 1
    let g:pymode_lint_unmodified = 1
    let g:pymode_lint_on_fly = 0  " lints on InsertLeave
    let g:pymode_lint_ignore = ["C0301", "E501"]
    let g:pymode_lint_sort = ['E', 'C', 'I']
    let g:pymode_folding = 1
    let g:pymode_syntax_slow_sync = 0
    let g:pymode_trim_whitespaces = 0
    let g:pymode_rope_complete_on_dot = 0

    let g:pymode_rope_rename_bind = g:IDE_mappings.rename
    let g:pymode_rope_rename_module_bind = g:IDE_mappings.renameModule
    let g:pymode_run_bind = g:IDE_mappings.REPLSend
    let g:pymode_breakpoint_bind = g:IDE_mappings.setBreakpoint
    let g:pymode_rope_prefix = g:IDE_mappings.codeAction
    let g:pymode_rope_goto_definition_bind = g:IDE_mappings.definition2
    " let g:pymode_rope_extract_method_bind = g:IDE_mappings.extractMethod
    " Jump to definition in current window
    let g:pymode_rope_goto_definition_cmd = 'e'
    augroup myPymode
        au!
        autocmd FileType python exec 'nnoremap <buffer> ' . g:IDE_mappings.lintBuffer . " <cmd>PymodeLint<CR>"
    augroup END


    " Folding can be slow so recalculating during every character entered is
    " foolish
    " augroup unset_folding_in_insert_mode
    "     autocmd!
    "     autocmd InsertEnter *.py noa setlocal foldmethod=marker
    "     autocmd InsertLeave *.py noa setlocal foldmethod=expr
    " augroup END

endif

let g:SimpylFold_docstring_preview = 1

if IsPluginUsed('jedi-vim')
    if IsPluginUsed('deoplete.nvim')
        let g:jedi#completions_enabled = 0
    endif
    let g:jedi#use_splits_not_buffers = "right"
    let g:jedi#goto_command = g:IDE_mappings.definition
    let g:jedi#goto_assignments_command = g:IDE_mappings.implementation
    let g:jedi#goto_definitions_command = g:IDE_mappings.definition
    let g:jedi#documentation_command = g:IDE_mappings.documentation
    let g:jedi#usages_command = g:IDE_mappings.references
    let g:jedi#completions_command = "<C-e>"
    let g:jedi#rename_command = g:IDE_mappings.rename
endif

if IsPluginUsed('jupytext.vim')
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
if IsPluginUsed('vim-r-plugin')
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
    " Plugin 'https://github.com/gaalcaras/ncm-R'
endif
" {]} ---------- R ----------

" {[} ------ C ------
if IsPluginUsed('vim-cpp-enhanced-highlight')
    let g:cpp_class_decl_highlight = 1
    let g:cpp_member_variable_highlight = 1
endif
if IsPluginUsed('c-support')
    let g:C_Ctrl_j = 'off'
endif
" {]} ------ C ------


" Advanced markdown formatting. Lots of features.
if IsPluginUsed('mkdx')
    let g:mkdx#settings = {
                \ 'enter':          { 'shift': 1 },
                \ 'map':            { 'prefix': '<localleader>', 'enable': 1 },
                \ 'toc':            { 'text': 'Table of Contents', 'update_on_write': 1 },
                \ 'fragment':       { 'complete': 0 },
                \ 'highlight':      { 'enable': 1 },
                \ 'fold':           { 'enable': 1 },
                \ 'auto_update':    { 'enable': 1 }
                \ }
endif

" {]} ---------- Lang-specific ----------

" {[} ---------- Git ----------
if IsPluginUsed('git-messenger.vim')
    let g:git_messenger_no_default_mappings=v:true
    " Include diff for all files in commit. Could be 'current'
    let g:git_messenger_include_diff="all"
    " Move cursor into popup for easier scrolling. Can manually do it by
    " running command a second time.
    let g:git_messenger_always_into_popup=v:true
endif
if IsPluginUsed('vimagit')
    nnoremap <space>gg <cmd>Magit<CR>
endif
" {]} ---------- Git----------

" {[} ---------- IDE ----------
if IsPluginUsed('vim-dadbod')
    let g:db_ui_save_location = g:vimfilesDir
endif
if IsPluginUsed('errormarker.vim')
    let &errorformat="%f:%l:%c: %t%*[^:]:%m,%f:%l: %t%*[^:]:%m," . &errorformat
    let errormarker_disablemappings = 1
    cabbrev er ErrorAtCursor
endif
if IsPluginUsed('vim-startify')
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
endif

if IsPluginUsed('vim-test')
    " make test commands execute using dispatch.vim
    let test#strategy = 'dispatch'
endif

if IsPluginUsed('neoformat')
    nnoremap gQ <cmd>Neoformat<CR>
    vnoremap gQ <cmd>Neoformat<CR>
endif

" {]} ---------- IDE----------

" {[} ---------- Debugging ----------
if IsPluginUsed('vim-unstack')
    " IDK what mapping to use here.
    let g:unstack_mapkey='<F2>'
    " Top to bottom splits
    let g:unstack_layout = 'portrait'
endif
if IsPluginUsed('vimspector')
    call SourcePluginFile('vimspector.vim')
endif
" {]} ---------- Debugging ----------

" {[} ---------- Completion ----------

if IsPluginUsed('SyntaxComplete')
    autocmd myPlugins Filetype *
                \	if &omnifunc == '' |
                \		setlocal omnifunc=syntaxcomplete#Complete |
                \	endif
    " This function gives list of completion items, for use in other
    " plugins.
    " let s:syntaxKeywords = OmniSyntaxList( [] )
endif

if IsPluginUsed('coc.nvim')
    call SourcePluginFile('coc.nvim.vim')
endif

if IsPluginUsed('copilot.vim')
    imap <silent><script><expr> <C-L> copilot#Accept('\<CR>')
    let g:copilot_no_tab_map = v:true
endif

if IsPluginUsed('ycm')
    call SourcePluginFile('ycm.vim')
endif
if IsPluginUsed('deoplete')
    call SourcePluginFile('deoplete.vim')
endif

if IsPluginUsed('completor.vim')
    " Use TAB to complete when typing words, else inserts TABs as usual.  Uses
    " dictionary, source files, and completor to find matching words to complete.

    " Check the plugin has loaded correctly before overriding
    " completion command.
    if exists('completor#do')
        let g:completionCommand = '\<C-R>=completor#do("complete")\<CR>'
    endif
    let g:completor_auto_trigger = 1
endif
if IsPluginUsed('vim-mucomplete')
    let g:mucomplete#enable_auto_at_startup = 1
    " Only pause after no typing for [updatetime]
    let g:mucomplete#delayed_completion = 1
    set completeopt+=menuone,noselect
endif
" {]} ---------- Completion----------
"
" {[} ---------- Snippits ----------
if IsPluginUsed('neosnippet-snippets')
    let g:neosnippet#enable_snipmate_compatibility=1
    " let g:neosnippet#enable_conceal_markers=0
    if !IsPluginUsed('coc.nvim')
        exec 'imap ' . g:IDE_mappings.snippetExpand . ' <Plug>(neosnippet_expand_or_jump)'
        exec 'vmap ' . g:IDE_mappings.snippetExpand . ' <Plug>(neosnippet_expand_or_jump)'
        exec 'imap ' . g:IDE_mappings.snippetNext . ' <Plug>(neosnippet_jump)'
        exec 'vmap ' . g:IDE_mappings.snippetNext . ' <Plug>(neosnippet_jump)'
    endif
    " imap <expr><TAB>
    "             \ pumvisible() ? "\<C-n>" :
    "             \ neosnippet#expandable_or_jumpable() ?
    "             \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
    " smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
    "             \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
endif
if IsPluginUsed('ultisnips')
    if !IsPluginUsed('coc.nvim')
        let g:UltiSnipsExpandTrigger = g:IDE_mappings.snippetExpand
        let g:UltiSnipsJumpForwardTrigger = g:IDE_mappings.snippetNext
        let g:UltiSnipsJumpBackwardTrigger = g:IDE_mappings.snippetPrev
    endif
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
endif
" {[} ---------- Snipmate ----------
if IsPluginUsed('vim-snipmate')
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
endif
" {]} ---------- Snipmate ----------
" {]} ---------- Snippits----------

" {[} ---------- REPL ----------
" {]} ---------- REPL ----------
