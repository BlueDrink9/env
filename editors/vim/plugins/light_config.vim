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

if IsPluginUsed("asyncrun.vim")
    " Open quickfix window at height 8 on running
    let g:asyncrun_open = 8
    let g:asyncrun_auto = "make"
    command! -bang -nargs=* -complete=file ASMake AsyncRun -program=make @ <args>
    cabbrev make Make
    cabbrev !! AsyncRun
    " Set qf statusbar to status of asyncrun
    let g:asyncrun_status = "stopped"
    autocmd myPlugins QuickFixCmdPost * call asyncrun#quickfix_toggle(8, 1)
    autocmd myPlugins BufWinEnter quickfix setlocal 
                \ statusline=%t\ [%{g:asyncrun_status}]\ %{exists('w:quickfix_title')?\ '\ '.w:quickfix_title\ :\ ''}\ %=%-15(%l,%c%V%)\ %P
endif


if IsPluginUsed("csv.vim")
    " Set to 0 to skip autoformatting of CSVs.
    let g:csv_autocmd_arrange	   = 1
    " limit to only apply for files under 1 MB
    let g:csv_autocmd_arrange_size = 1024*1024
    " let g:csv_highlight_column = 'y' " Current cursor's column.
    " hi CSVColumnEven term=bold ctermbg=Gray guibg=LightGray
    " TODO link to something so this doesn't look awful outside solarized light.
    call add (g:customHLGroups, "CSVColumnEven guibg=gray90 ctermbg=lightgray")
    " TODO Check if csv column highlight should be a highlight update or a
    " pluginSettings update.
    highlight clear CSVColumnOdd
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
if IsPluginUsed("yanky.nvim")
    lua << EOF
    require("yanky").setup({
    system_clipboard = {
        sync_with_ring = false,
        },
        highlight = {
            on_put = true,
            on_yank = true,
            timer = 300,
        },
    })
   vim.keymap.set({"n","x"}, "p", "<Plug>(YankyPutAfter)")
   vim.keymap.set({"n","x"}, "P", "<Plug>(YankyPutBefore)")
   vim.keymap.set({"n","x"}, "<leader>p", "<Plug>(YankyCycleForward)")
   vim.keymap.set({"n","x"}, "<leader>P", "<Plug>(YankyCycleForward)")
   -- Unimpaired style
   vim.keymap.set("n", "]p", "<Plug>(YankyPutIndentAfterLinewise)")
   vim.keymap.set("n", "[p", "<Plug>(YankyPutIndentBeforeLinewise)")
   vim.keymap.set("n", "]P", "<Plug>(YankyPutIndentAfterLinewise)")
   vim.keymap.set("n", "[P", "<Plug>(YankyPutIndentBeforeLinewise)")

   vim.keymap.set("n", ">p", "<Plug>(YankyPutIndentAfterShiftRight)")
   vim.keymap.set("n", "<p", "<Plug>(YankyPutIndentAfterShiftLeft)")
   vim.keymap.set("n", ">P", "<Plug>(YankyPutIndentBeforeShiftRight)")
   vim.keymap.set("n", "<P", "<Plug>(YankyPutIndentBeforeShiftLeft)")

   vim.keymap.set("n", "=p", "<Plug>(YankyPutAfterFilter)")
   vim.keymap.set("n", "=P", "<Plug>(YankyPutBeforeFilter)")
EOF
elseif IsPluginUsed("vim-yoink")
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
endif
if IsPluginUsed("vim-operator-replace")
    nmap dr  <Plug>(operator-replace)
    nmap drr 0<Plug>(operator-replace)$
endif


if IsPluginUsed("vim-ReplaceWithRegister")
    " Causes delay on yank op.
    " xmap yr  <Plug>ReplaceWithRegisterVisual
    xmap R  <Plug>ReplaceWithRegisterVisual
endif
" {]}--- Operators ---

" {[}--- Visual changes ---
if IsPluginUsed("eyeliner.nvim")
    lua require'eyeliner'.setup { highlight_on_key = true, dim = true }
elseif IsPluginUsed("quick-scope")
    " Trigger a highlight in the appropriate direction when pressing these keys:
    let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
endif

if IsPluginUsed("vim-better-whitespace")
    let g:better_whitespace_filetypes_blacklist=['diff', 'gitcommit', 'unite', 'qf', 'help', 'far']
    let g:strip_whitespace_confirm=0
    let g:show_spaces_that_precede_tabs=1
    let g:strip_whitespace_on_save = 1
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
    " Scratch isn't explicitly a prose ft, but for quick notes I want it treated
    " as one.
    augroup lightPencil
        au!
        autocmd CursorHold * if &filetype == "scratch" | call pencil#init() | au! lightPencil
    augroup end
endif
" {]} ---------- Prose----------
