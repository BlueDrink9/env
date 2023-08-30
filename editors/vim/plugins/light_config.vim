" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" {[} Litemode only / replacements.
if IsPluginUsed("lightline.vim")
    " Only show buffer line if there are > 2 buffers open.
    let g:buftabline_show=1
    let g:buftabline_numbers=2
endif

if IsPluginUsed("vim-commentary")
    " Normal commentary maps (eg gcc) only happen if gc isn't mapped or
    " Commentary isn't already re-mapped.
    xmap <leader>c  <Plug>Commentary
    nmap <leader>c  <Plug>Commentary
    omap <leader>c  <Plug>Commentary
    nmap <leader>cc <Plug>CommentaryLine
    nmap <leader>c <Plug>ChangeCommentary
    nmap <leader>cu <Plug>Commentary<Plug>Commentary
    xmap <C-/>  <Plug>Commentary
    nmap <C-/>  <Plug>Commentary
    omap <C-/>  <Plug>Commentary
endif

" {[}--- Misc ---
if IsPluginUsed("FastFold")
    let g:tex_fold_enabled=1
    let g:vimsyn_folding='af'
    let g:xml_syntax_folding = 1
    let g:javaScript_fold = 1
    let g:ruby_fold = 1
    let g:sh_fold_enabled= 7
    let g:php_folding = 1
    let g:perl_fold = 1
    let g:python_fold = 1
    let g:fastfold_fold_command_suffixes =
                \ ['x','X','a','A','o','O','c','C','r','R','m','M','i','n','N']
    nnoremap <BS> <cmd><C-u>FastFoldUpdate<cr>za
    let g:fastfold_minlines = 700
endif

if IsPluginUsed("vim-fakeclip")
    nmap "" "+
    vmap "" "+
    " In insert or visual mode, use standard cut/copy/paste shortcuts.
    imap <C-v> <Plug>(fakeclip-insert)
    cmap <C-v> <Plug>(fakeclip-insert)
    vmap <C-X> <Plug>(fakeclip-d)
    vmap <C-c> <Plug>(fakeclip-y)
endif

" {[}--- Yanks ---
if IsPluginUsed("vim-yoink")
    if has('nvim')
        let g:yoinkSavePersistently = 1  " Nvim only.
    endif
    let g:yoinkSwapClampAtEnds = 0  " Cycle back to start.
    let g:yoinkSyncSystemClipboardOnFocus = 0

    nmap <expr> <leader>p yoink#canSwap() ?
                \ '<plug>(YoinkPostPasteSwapBack)' :
                \ '<plug>(YoinkPaste_p)'
    nmap <expr> <leader>P yoink#canSwap() ?
                \ '<plug>(YoinkPostPasteSwapForward)' :
                \ '<plug>(YoinkPaste_P)'
elseif IsPluginUsed("vim-yankstack.git")
    let g:yankstack_yank_keys = ['c', 'C', 'd', 'D', 'x', 'X', 'y', 'Y']
    call yankstack#setup()
    nmap <leader>p <Plug>yankstack_substitute_older_paste
    nmap <leader>P <Plug>yankstack_substitute_newer_paste
endif
" {]}--- Yanks ---

" {[}--- Operators ---
if IsPluginUsed("vim-unimpaired")
    " I want to use ]e (normally move lines up and down) for errors.
    let g:nremap = {"[e": "", "]e": ""}
    let g:xremap = {"[e": "", "]e": ""}
    let g:oremap = {"[e": "", "]e": ""}
    " Mimic doom emacs. "Toggle"
    nmap <leader>t yo
endif

if IsPluginUsed("vim-ReplaceWithRegister")
    nmap dr  <Plug>ReplaceWithRegisterOperator
    nmap drr <Plug>ReplaceWithRegisterLine
    " Causes delay on yank op.
    " xmap yr  <Plug>ReplaceWithRegisterVisual
    xmap R  <Plug>ReplaceWithRegisterVisual
elseif IsPluginUsed("vim-operator-replace")
    nmap dr  <Plug>(operator-replace)
    nmap drr 0<Plug>(operator-replace)$
endif
" {]}--- Operators ---

" {[}--- Visual changes ---
if IsPluginUsed("quick-scope")
    " Trigger a highlight in the appropriate direction when pressing these keys:
    let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
endif

if IsPluginUsed("vim-better-whitespace")
    let g:better_whitespace_filetypes_blacklist=['diff', 'gitcommit', 'unite', 'qf', 'help', 'far']
    let g:strip_whitespace_confirm=0
    let g:show_spaces_that_precede_tabs=1
    " uses vimrun.exe, which is slow
    if has('nvim') || !has('win32')
       let g:strip_whitespace_on_save = 1
    endif
    let g:strip_only_modified_lines=1
    let g:better_whitespace_skip_empty_lines=0
    " Don't HL current line. May cause performance issues.
    let g:current_line_whitespace_disabled_hard=0
    let g:better_whitespace_operator=''
    " call add (g:customHLGroups, "ExtraWhitespace ctermbg=Gray guibg=LightGray")
    call add (g:customHLGroups, "link ExtraWhitespace Visual")
    " call add (g:customHLGroups, "link ExtraWhitespace SpecialKey")
    function! WhitespaceHighlightDisable()
        DisableWhitespace
    endfunction
    autocmd myPlugins BufReadPost * if &readonly || ! &modifiable | call WhitespaceHighlightDisable() | endif
endif
" {]}--- Visual ---

" {[} --- TMUX ---
if IsPluginUsed("vim-tmux")
    " Prompt for a command to run
    map <Leader>vp <cmd>VimuxPromptCommand<CR>
    " Run last command executed by VimuxRunCommand
    map <Leader>vl <cmd>VimuxRunLastCommand<CR>
endif
" {]} TMUX

" {[} ---------- Prose ----------
if IsPluginUsed("vim-pencil")
    let g:pencil#wrapModeDefault = 'soft'
    let g:pencil#conceallevel=&conceallevel
    let g:pencil#concealcursor=&concealcursor
endif
" {]} ---------- Prose----------
