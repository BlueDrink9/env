" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" {[} ---------- Providers/External model setup neovim ----------
let g:skipPythonInstall=1  " Tmp skip installing python modules.
" Install python module, preferably for py3.
function! PythonInstallModule(module)
    if exists('g:skipPythonInstall')
        return
    endif
    if !exists('g:pyInstaller')
        if executable('conda')
            let g:pyInstaller="conda install -y -c conda-forge -c malramsay "
        else
            if executable('pip3')
                let l:pipVersion="pip3"
            elseif executable('pip')
                " elseif executable('pip') && system('pip --version') =~ '3'
                let g:pyInstaller="pip"
                " Fallback - python 2
            elseif executable('pip2')
                let g:pyInstaller="pip2"
            else
                silent echom "Err: No pip installed. Will not install python support."
                return
            endif
            let g:pyInstaller=l:pipVersion . " install --user --upgrade "
        endif
    endif
    exec "!" . g:pyInstaller . a:module
endfunction

" Should pick up from either python types.
if !exists('g:skipPythonInstall')
    if has('nvim') && !(has("python") || has("python3"))
        " Needed for neovim python support.
        call PythonInstallModule('neovim')
    endif
endif
" {]} ---------- Module setup ----------

" {[} ---------- Misc ----------
if IsPluginUsed("vim-auto-origami")
    au myPlugins CursorHold,BufWinEnter,WinEnter * AutoOrigamiFoldColumn
endif
if IsPluginUsed("supertab")
    let g:SuperTabDefaultCompletionType = "context"
    " Fallback for context.
    let g:SuperTabContextDefaultCompletionType = "<c-p>"
    let g:SuperTabLongestEnhanced = 1
    let g:SuperTabMappingForward='<tab>'
    let g:SuperTabMappingBackward='<s-tab>'
    let g:SuperTabLongestEnhanced=1
    let g:SuperTabClosePreviewOnPopupClose = 1
" List of omni completion option names in the order of precedence that they should be used if available
" let g:SuperTabContextTextOmniPrecedence = ['&completefunc', '&omnifunc']
endif

if IsPluginUsed("tcomment_vim")
    let g:tcomment_opleader1='<leader>c'
    " By default <c-_>, it is what terminals send for <c-/>. That's what a lot of
    " IDEs use, so it's probably worth keeping... but it also happens to be what
    " mintty sends as <c-bs>. Gvim doesn't send anything.
    " Maps in insert mode.
    if $TERM_PROGRAM ==? "mintty"
        let g:tcomment_mapleader1=''
    endif
    " default '<leader>_'. Maps in select mode. I don't use it.
    let g:tcomment_mapleader2=''
    let g:tcomment#blank_lines=0
    xnoremap <C-/>  <cmd>Tcomment<CR>
    nnoremap <C-/>  <cmd>TcommentBlock<CR>
    onoremap <C-/>  <cmd>Tcomment<CR>
endif
if IsPluginUsed("vim-showmarks.git")
    " More advanced version of showmarks. Lots of mappings, eg m]
    Plug 'jeetsukumaran/vim-markology', {'on': ['MarkologyEnable', 'MarkologyToggle']}
    nnoremap m? <cmd>MarkologyEnable<cr>
    let g:markology_enable=0
    let g:markology_ignore_type="hpq"
    let g:markology_include=
                \ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.'`^<>[]()\""
    call add (g:customHLGroups, "link MarkologyHLl LineNr")
    call add (g:customHLGroups, "link MarkologyHLu LineNr")
    call add (g:customHLGroups, "link MarkologyHLo LineNr")
    " call add (g:customHLGroups, "link MarkologyHLo LineNr")
endif

if IsPluginUsed("vim-mundo")
    cabbrev undo MundoToggle
endif

if IsPluginUsed("vim-CtrlXA")
    nmap <Plug>SpeedDatingFallbackUp   <Plug>(CtrlXA-CtrlA)
    nmap <Plug>SpeedDatingFallbackDown <Plug>(CtrlXA-CtrlX)
endif

if IsPluginUsed("vinarise.vim")
    let g:vinarise_enable_auto_detect=1
    let g:vinarise_detect_large_file_size=0
    " dump with VinarisePluginDump
    " let g:vinarise_objdump_command="objdump"
    let g:vinarise_objdump_intel_assembly=1

    au myPlugins FileType vinarise call s:setupVinarise()
    function! s:setupVinarise()
        command! Assembly VinarisePluginDump
        command! Dump VinarisePluginDump
        DisableWhitespace
    endfunction
endif
if IsPluginUsed("hexmodee")
    command! HexEdit :HexMode<CR>
    let g:hexmode_autodetect = 1
    let g:hexmode_patterns = '*.bin,*.exe,*.dat,*.o'
endif
if IsPluginUsed("pfp-vim")
    command! HexEditFull :PfpParse<CR>
endif

if IsPluginUsed("guess-indent.nvim")
    lua require('guess-indent').setup {}
elseif IsPluginUsed("detectindent")
    let g:detectindent_preferred_expandtab = &expandtab
    let g:detectindent_preferred_indent = &shiftwidth
    au myPlugins BufReadPost * silent! :DetectIndent
endif

" Zoom window to tab, and out again
if IsPluginUsed('zoomwintab.vim')
    nnoremap <c-w>z <cmd>ZoomWinTabToggle<cr>
endif

if IsPluginUsed('bufexplorer')
  let g:bufExplorerDisableDefaultKeyMapping=1
  command! Buffers BufExplorer
endif
" {]} ---------- Misc----------

" {[} ---------- Visual changes ----------
if IsPluginUsed("vim-highlightedyank")
    if !exists('##TextYankPost')
        map y <Plug>(highlightedyank)
    endif
    " -1 gives persistent highlight until edit or new yank.
    let g:highlightedyank_highlight_duration = 3000
endif
" {]} ---------- Visual changes ----------

" {[} View and session
if IsPluginUsed("vim-stay")
    set viewoptions=cursor,folds,slash,unix
endif
if IsPluginUsed("vim-session")
    let g:session_directory = CreateVimDir("sessions")
    let g:session_persist_globals = ['&spelllang', '&spell', '&autoread']
    let g:session_persist_colors = 0
    let g:session_persist_font = 0
    " Open recent session instead of default
    let g:session_default_to_last = 'yes'
    " Auto-save unnamed sessions as default
    let g:session_default_overwrite = 1
    let g:session_autosave_periodic = 10
    let g:session_autosave = 'yes'
    let g:session_autosave_silent = 1
    let g:session_autoload = 'no' " Could also be 'prompt'
    let g:session_verbose_messages = 0 " Affects load/save prompts
    Alias cs CloseSession
    Alias os OpenSession
    Alias ss SaveSession
endif
" {]} View and session

" {[} Extra text objects
if IsPluginUsed("targets.vim")
    " Don't handle argument. Use another plugin
    autocmd User targets#mappings#user call targets#mappings#extend({
                \ 'a': {},
                \ })
endif
if IsPluginUsed("vim-textobj-sentence")
    " Called by vim-plug when this is loaded.
    autocmd! User vim-textobj-sentence call textobj#sentence#init()
endif
if IsPluginUsed("vim-textobj-entire")
    let g:textobj_entire_no_default_key_mappings=1
    omap a% <Plug>(textobj-entire-a)
    vmap a% <Plug>(textobj-entire-a)
    omap i% <Plug>(textobj-entire-i)
    vmap i% <Plug>(textobj-entire-i)
endif
" {]} Extra text objects

" {[} ---------- Operators ----------
if IsPluginUsed("vim-sandwich")
    " Gives it tpope-surround mappings.
    runtime macros/sandwich/keymap/surround.vim
    vmap s <Plug>(operator-sandwich-add)
endif
if IsPluginUsed("hop.nvim")
    lua require'hop'.setup({ keys = 'arstdhneofplucmkv'})
lua << EOF
    function map(mode, keys, mapping, opts)
        vim.api.nvim_set_keymap(mode, keys, mapping, opts)
        end
        v = {'x', 'n'}
        for i = 1, #v do
            mode = v[i]
            map(mode, ',l', "<cmd>lua require'hop'.hint_words({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR })<cr>", {})
            map(mode, ',h', "<cmd>lua require'hop'.hint_words({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR })<cr>", {})
            map(mode, ',j', "<cmd>lua require'hop'.hint_lines({ direction = require'hop.hint'.HintDirection.AFTER_CURSOR })<cr>", {})
            map(mode, ',k', "<cmd>lua require'hop'.hint_lines({ direction = require'hop.hint'.HintDirection.BEFORE_CURSOR })<cr>", {})
        end
EOF
endif
if IsPluginUsed("vim-easymotion")
    let g:EasyMotion_do_mapping = 0 " Disable default mappings
    " tab-incrementable search with easymotion dropout feature.
    " map  <leader>/ <Plug>(easymotion-sn)
    " omap <leader>/ <Plug>(easymotion-tn)
    let g:EasyMotion_smartcase = 1
    map ,l <Plug>(easymotion-lineforward)
    map ,j <Plug>(easymotion-j)
    map ,k <Plug>(easymotion-k)
    map ,h <Plug>(easymotion-linebackward)
    let g:EasyMotion_startofline = 0 " keep cursor column when JK motion
    " Cross window boundaries
    nmap <Leader><S-K> <Plug>(easymotion-overwin-line)
    nmap <Leader><S-J> <Plug>(easymotion-overwin-line)
    nmap <Leader><S-L> <Plug>(easymotion-overwin-w)
    nmap <Leader><S-H> <Plug>(easymotion-overwin-w)
    " Like sneak
    nnoremap <leader>s <Plug>(easymotion-overwin-f2)
endif
if IsPluginUsed("camelcasemotion")
    call camelcasemotion#CreateMotionMappings('S')
    call camelcasemotion#CreateMotionMappings(',')
endif
" {]} ---------- Operators ----------

" {[}--- Searching, replacing, finding ---
if IsPluginUsed("vim-grepper")
    let g:grepper = {
                \ 'tools': ['rg', 'ag', 'ack', 'findstr', 'pt', 'git', 'grep'],
                \ }
    cabbrev bfind Grepper -query -noprompt
    nnoremap <leader>*  :Grepper -cword -noprompt<cr>
    command! -nargs=1 Grep Grepper -noprompt -query <args>
    command! -nargs=1 Rg Grepper-noprompt -query <args>

endif
if IsPluginUsed("FlyGrep.vim")
    cabbrev bsearch FlyGrep
    nnoremap <leader>/ <cmd>FlyGrep<CR>
    let g:FlyGrep_input_delay = 200  " ms. default 500
endif
" x - exclude. i - include. t - toggle. Capital X I T for all.
if IsPluginUsed("far.vim")
    command! Replace Farp
    " Project replace.
    " nnoremap <leader>pr <cmd>Farp<CR>
    function! s:farMappings()
        nnoremap <buffer><silent> q <cmd>call g<cmd>far#close_preview_window()<cr>
        nnoremap <buffer><silent> <bs> <cmd>call g<cmd>far#change_collapse_under_cursor(-1)<cr>
        nnoremap <buffer><silent> <c-CR> <cmd>Fardo<CR>
        nnoremap <buffer><silent> W <cmd>Refar<CR>
        nnoremap <buffer><silent> r <cmd>Fardo<CR>
    endfunction
    autocmd myPlugins FileType far call s:farMappings()
    let g:far#default_mappings=1
    let g:far#auto_preview=1  " Autoscroll
    let g:far#window_layout='current'
    " Used for completion
    let g:far#file_mask_favorites=['**/*.*', '%']
    let g:far#default_file_mask='**/*.*'
    " Sets the first one in the loop it finds.
    for p in ['rg', 'ag', 'ack']
        if executable(p)
            if has('nvim')
                let g:far#source=p . 'nvim'
            else
                let g:far#source=p
            endif
            break
        endif
    endfor
endif

if IsPluginUsed("vim-buffersaurus")
    cabbrev bfind Bsgrep
    command! Find Bsgrep
    command! Replace2 Bsreplace
    let g:buffersaurus_autodismiss_on_select=0
endif

" {[}--- Fuzzy finder ---
if IsPluginUsed("ctrlp.vim")
    " Look in buffers, files and MRU.
    let g:ctrlp_cmd = 'CtrlPMixed'
    if executable('fd')
        let g:ctrlp_user_command = 'fd %s -type f'
    endif
    let g:ctrlp_map = '<leader><space>'
    let g:ctrlp_cache_dir = CreateVimDir("ctrpCache") " Purge cache with f5 in buffer
    let g:ctrlp_clear_cache_on_exit = 0
    if ideMode == 1
        let g:ctrlp_extensions = ['tag', 'buffertag', 'rtscript']
    endif
endif
if IsPluginUsed("fzf")
    call SourcePluginFile("fzf.vim")
endif
if IsPluginUsed("telescope.nvim")
    call SourcePluginFile("telescope.lua")
endif
" {]}--- Fuzzy finder ---
" {]}--- Searching, replacing, finding ---

" {[} Tags
if IsPluginUsed("vim-todo")
    command! Todo OpenTodo
endif
if IsPluginUsed("TaskList.vim")
    " Need a mapping to prevent it auto-creating and overriding <leader>t
    nmap <Plug>__blank <Plug>TaskList
    command! Todo TaskList
endif
" {]}

"{[} Running/executing
if IsPluginUsed("vim-dispatch")
    call Nnoremap(g:IDE_mappings.make, "<cmd>w <bar> Dispatch<cr>")
    let g:dispatch_no_maps = 1
    call Nmap(g:IDE_mappings.make, "<cmd>Make<CR>")
endif
"{]}

" {[} ---------- Basic extra filetype support ----------
if IsPluginUsed("graphviz.vim")
    " Autocompile dotfile on write if fewer than 50 lines.
    autocmd myPlugins bufwritepost *.dot if line("$") < 50 | GraphvizCompile | endif
endif

if IsPluginUsed("vim-openscad")
    lua require('openscad')
    let g:openscad_load_snippets = false
    let g:openscad_auto_open = false
    let g:openscad_default_mappings = false
endif

" {]} ---------- Basic extra filetype support ----------

" {[} ---------- Git ----------
if IsPluginUsed("vim-fugitive")
    " nnoremap <leader>gs <cmd>Gstatus<CR> cabbrev gs Gstatus
    cabbrev gs Git
    cabbrev gw Gwrite
    cabbrev gc Gwrite <bar> Git commit
    cabbrev gco Git commit
    cabbrev gupw Gwrite <bar> Git commit --amend --no-edit
    cabbrev gup Git commit --amend --no-edit
    cabbrev gupe Git commit --amend
    cabbrev gd Gdiff
    cabbrev gps Git push
    cabbrev gpl Git pull
    " mappings to emulate doom magit
    nnoremap <leader>gg <cmd>Git<cr>
    nnoremap <leader>gs <cmd>Gwrite<cr>
    nnoremap <leader>gcc <cmd>Gwrite <bar>  Git commit<cr>
    nnoremap <leader>gcc <cmd>Gwrite <bar>  Git commit<cr>
    nnoremap <leader>gce <cmd>Gwrite <bar> Git commit --amend --no-edit<cr>
    nnoremap <leader>gl <cmd>Git pull<cr>
    nnoremap <leader>gp <cmd>Git push<cr>
    " These are deprecated in fugitive itself.
    command! -bang -nargs=* Gpush Git push <args>
    command! -bang -nargs=* Gpull Git pull <args>
    autocmd myPlugins filetype fugitive setlocal nobuflisted

    " Async Fugitive (Fugitive uses whatever `Make` is available).
    if IsPluginUsed("asyncrun.vim")
        command! -bang -nargs=* -complete=file
                \ Make AsyncRun -program=make @ <args>
    endif
endif
" Enhances working with branches in fugitive "
if IsPluginUsed('gv.vim')
    command! Glog :GV<CR>
endif

if IsPluginUsed('gitsigns.nvim')
lua << EOF
require('gitsigns').setup({
    signs = {
        add = {hl = 'GitSignsAdd', text = '+', numhl='GitSignsAddNr', linehl='GitSignsAddLn'},
    },
    signcolumn = true,
    numhl      = true,
})
EOF
endif

if IsPluginUsed("vim-signify")
    call Nmap(g:IDE_mappings.VCSNextHunk, "<Plug>(signify-next-hunk)")
    call Vmap(g:IDE_mappings.VCSPreviousHunk, "<Plug>(signify-prev-hunk)")
    " Add VCS systems to this when needed. More will slow buffer loading.
    let g:signify_vcs_list = [ 'git' ]
    " Async, so shouldn't be too bad. Ignored if not async.
    " let g:signify_realtime = 1
    " Causes a write on cursorhold. PITA, so let's replace it.
    " autocmd myPlugins User pluginSettingsToExec autocmd! signify CursorHold,CursorHoldI
    if has('timers')
        autocmd User Fugitive silent! SignifyRefresh
        " This seems to be causing an annoying error :/
        " autocmd myPlugins CursorHold,CursorHoldI,BufEnter,FocusGained call silent! sy#start()
        " autocmd myPlugins WinEnter call silent! sy#start()
    endif

    autocmd User SignifyHunk call s:show_current_hunk()

    function! s:show_current_hunk() abort
        let h = sy#util#get_hunk_stats()
        if !empty(h)
            echo printf('[Hunk %d/%d]', h.current_hunk, h.total_hunks)
        endif
    endfunction

    " let g:signify_update_on_focusgained = 1
    let g:signify_sign_show_count = 1
    let g:signify_sign_change = '~'
    " Looks good and is effective with colour highlighting. Not too
    " distracting.
    let g:signify_sign_add                  = '┃'
    let g:signify_sign_delete               = '┃'
    let g:signify_sign_change               = '┃'

    if hlexists('LineNr')
        let g:signify_number_highlight = 1
        let g:signify_priority = 1
    endif
endif

if IsPluginUsed("vim-gitgutter")
    " Allows hlcolumn bg to match coloursch
    call add(g:customHLGroups, "clear SignColumn")
    " gitgutter needs grep to not output escap sequences.
    " let g:gitgutter_grep = ''
    let g:gitgutter_grep = 'grep --color=never'
    let g:gitgutter_override_sign_column_highlight = 0
    let g:gitgutter_escape_grep = 1
    " Disable automatic update
    autocmd! gitgutter CursorHold,CursorHoldI
    " " Wait 2000 ms after typing finishes before updating (vim default 4000)
    " set updatetime=2000
    au myPlugins BufWritePost * :GitGutter
    " Speed issues
    " plugin only runs on BufRead, BufWritePost and FileChangedShellPost, i.e. when you open or save a file.
    let g:gitgutter_realtime = 0
    let g:gitgutter_eager = 0
endif

if IsPluginUsed("vim-conflicted")
    function! s:setupConflicted()
        " TODO: integrate with airline.
        set stl+=%{ConflictedVersion()}
        " Resolve and move to next conflicted file.
        nnoremap ]m <cmd>GitNextConflict<cr>
        nnoremap [m <cmd>GitPrevConflict<cr>
    endfunction
    let g:diffget_local_map = 'gl'
    let g:diffget_upstream_map = 'gu'
    autocmd myVimrc User VimConflicted call s:setupConflicted()
endif
" {]} ---------- Git----------

" {[} ---------- Prose ----------

" Better prose spellchecking
if IsPluginUsed("vim-lexical")
    let g:lexical#spell_key = '<localleader>ls'
    let g:lexical#thesaurus_key = '<localleader>lt'
    let g:lexical#dictionary_key = '<localleader>ld'
endif

" Pencil loaded in lite, for scratch.
if IsPluginUsed("vim-pencil")
    " let g:pencil#wrapModeDefault = 'soft'
    " let g:pencil#conceallevel=&conceallevel
    " let g:pencil#concealcursor=&concealcursor
    let g:pencil#autoformat_blacklist = [
                \ 'markdownCode',
                \ 'markdownUrl',
                \ 'markdownIdDeclaration',
                \ 'markdownLinkDelimiter',
                \ 'markdownHighlight[A-Za-z0-9]+',
                \ 'mkdCode',
                \ 'mkdIndentCode',
                \ 'markdownFencedCodeBlock',
                \ 'markdownInlineCode',
                \ 'mmdTable[A-Za-z0-9]*',
                \ 'txtCode',
                \ 'texMath',
                \ ]

    function! s:setProseOptions()
        " Add dictionary completion. Requires setting 'dictionary' option.
        setlocal complete+=k
        " Default spelling lang is En, I want en_nz.
        if &spelllang == "en"
            " Custom lang not set.
            setl spell spl=en_nz
        endif
        call pencil#init()
        " Undo pencil mappings to stop them overriding my own (since they are
        " buffer maps, they take precedence even though mine were mapped later)
        unmap <buffer> <up>
        unmap <buffer> <down>
        setl ai
    endfunction
    function! s:prepareProseOptionSet()
        augroup setProseOptions
            au!
            autocmd cursorhold <buffer> call <sid>setProseOptions() | au! setProseOptions
    endfunction

    exec 'autocmd myPlugins Filetype ' . join(g:proseFileTypes, ",") . ' call <sid>prepareProseOptionSet()'
endif

" {[} ---------- Vimtex ----------
if IsPluginUsed("vimtex")
    " autocmd myPlugins User pluginSettingsToExec exec "let g:vimtex_compiler_latexmk.build_dir = 'latexbuild'"
    let g:vimtex_compiler_progname = 'nvr'
    let g:vimtex_fold_enabled = 1
    let g:vimtex_fold_manual = 1 " instead of expr folding. Speeds up.
    " Omnifunc complete citations, labels, filenames, glossary
    let g:vimtex_complete_enabled = 1
    let g:vimtex_imaps_disabled = []
    let g:vimtex_compiler_latexmk = {
                \ 'build_dir' : 'latexbuild',
                \}
    " This prevents errors from showing :/
    " \ 'options' : [
    " \ ],
    " \}
    " let g:Tex_DefaultTargetFormat="pdf"
    if has('win32')
        let g:vimtex_view_general_viewer = 'SumatraPDF'
        let g:vimtex_view_general_options
                    \ = '-reuse-instance -forward-search @tex @line @pdf'
        let g:vimtex_view_general_options_latexmk = '-reuse-instance'
    endif
    if has('python3')
        " may need to add python site module bin to PATH if not already.
        " python3 -c 'import site; print(site.USER_BASE, end=\"\")'
        if has('nvim') && !has('clientserver')
            " May require specific configuration
            if executable('nvr')
                let g:vimtex_compiler_progname="nvr"
            else
                augroup vimtexWarning
                    au!
                    autocmd Filetype tex
                            \echom 'nvr not on PATH, so vimtex cannot do compiler callbacks. Install it with pip or conda'
                            \ | au! vimtexWarning
                augroup end
            endif
        endif
    endif

    function! SetVimtexMappings()
        " Ensure clean doesn't immediately get overridden...
        nnoremap <buffer> <localleader>lc <cmd>VimtexStop<bar>VimtexClean<cr>
    endfunction
    " autocmd myPlugins Filetype *tex set foldmethod=expr
    autocmd myPlugins Filetype tex :call SetVimtexMappings()
endif
" {]} ---------- Vimtex ----------

" {[} ---------- Markdown Preview ----------
if IsPluginUsed("markdown-preview.nvim")
    let g:mkdp_auto_start = 0
    let g:mkdp_auto_close = 0
    let g:mkdp_echo_preview_url = 1
    " On save, insertleave
    " let g:mkdp_refresh_slow = 1
endif
if IsPluginUsed("vim-markdown-preview")
    let vim_markdown_preview_toggle=2
    let vim_markdown_preview_hotkey='<localleader>r'
    command! MarkdownPreview :call Vim_Markdown_Preview()<CR>
    let vim_markdown_preview_pandoc=1
endif
" {]} ---------- Markdown Preview ----------

if IsPluginUsed("bullets.vim")
    let g:bullets_enabled_file_types = [
                \ 'markdown',
                \ 'text',
                \ 'gitcommit',
                \ 'scratch'
                \]
    let g:bullets_enable_in_empty_buffers = 0
endif
if IsPluginUsed("md-img-paste.vim")
    autocmd myPlugins FileType markdown command! PasteImage silent call mdip#MarkdownClipboardImage()<CR>
endif
" {]} ---------- Prose----------

" {[} ---------- Terminal ----------

if IsPluginUsed("toggleterm.nvim")
lua << EOF
    opts ={
    open_mapping = [[<c-s>]],
    direction = 'horizontal',
    start_in_insert = true,
    insert_mappings = false,
    terminal_mappings = true,
    persist_mode = true,
    close_on_exit = false, -- Otherwise may miss startup errors
    auto_scroll = false,
    shell = vim.o.shell,
    }
    if vim.fn.has("win32") == 1 then
        opts.shell = "powershell.exe"
    end
    require("toggleterm").setup(opts)
-- :ToggleTermSendVisualSelection 
EOF
endif

if IsPluginUsed("vim-terminal-help")
    " which key will be used to toggle terminal window, default to <m-=>.
    " Will be mapped while using term, so choose carefully. 
    let g:terminal_key="<c-S>"
    " initialize working dir: 0 for unchanged, 1 for file path and 2 for project root.
    let g:terminal_cwd=1
    " how to open the file in vim? default to tab drop.
    " let g:terminal_edit="e"
    " set to term to kill term session when exiting vim.
    let g:terminal_kill="term"
    " set to 0 to hide terminal buffer in the buffer list
    let g:terminal_list=0
    " Don't set up alt key for use. Fixes wierd strings in macvim terminal.
    let g:terminal_skip_key_init=1
    if has("win32")
        let g:terminal_shell="powershell.exe"
    endif
endif
" {]} ---------- Terminal ----------

" {[} ---------- NerdTree Project/file drawer ----------
if IsPluginUsed("nerdtree.git")
    " Change these if you feel the desire...
    let g:NERDTreeIndicatorMapCustom = {
                \ "Modified"  : "*",
                \ "Staged"    : "+",
                \ "Untracked" : "\",
                \ "Renamed"   : "➜",
                \ "Unmerged"  : "═",
                \ "Deleted"   : "×",
                \ "Dirty"     : "~",
                \ "Clean"     : "✔︎",
                \ 'Ignored'   : 'i',
                \ "Unknown"   : "?"
                \}
    " Open nerdtree when you :edit a directory
    let NERDTreeHijackNetrw=1
    let NERDTreeDirArrows = 1
    let NERDTreeShowHidden=1
    " Don't lose alternate file etc if NT opened.
    let NERDTreeCreatePrefix='silent keepalt keepjumps'
    " Open nerdtree on directory edit (startup)
    autocmd myPlugins StdinReadPre * let s:std_in=1
    autocmd myPlugins VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif
    autocmd myPlugins BufRead * if isdirectory(@%) | exec 'NERDTree' | endif
    autocmd myPlugins bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

    cabbrev nt NERDTreeToggle
    " nnoremap _ <cmd>NERDTreeToggle<CR>
    if g:useNerdFont
        let g:NERDTreeDisableFileExtensionHighlight = 1
        let g:NERDTreeDisableExactMatchHighlight = 1
        let g:NERDTreeDisablePatternMatchHighlight = 1
    endif
endif
" {]} ---------- NerdTree ----------

" {[} Neovim UIs/integrations
if exists('g:vscode')
    call SourcePluginFile("vscode-neovim.vim")
endif

if IsPluginUsed("firenvim")
    call SourcePluginFile("firenvim.vim")
endif

" {]} Neovim UIs/integrations

