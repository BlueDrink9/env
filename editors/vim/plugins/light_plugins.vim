" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" {[} Litemode only / replacements.
if g:liteMode
" Superlight airline (no plugins)
    " Plug 'https://github.com/itchyny/lightline.vim'
    " {[} Replace Tcomment with commentary
    " Replaced in favour of slightly heavier version tcomment in main
    " plugins. See https://github.com/wincent/wincent/commit/913e79724456976549244893e9025aa6fcf3cc1c
    Plug 'https://github.com/tpope/vim-commentary'
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
    " {]} Replace Tcomment with commentary
    " Use better 'vim-sandwich' in main.
    Plug 'https://github.com/tpope/vim-surround.git'
    " Lighter alt to airline for putting buffers in tabline.
    Plug 'https://github.com/ap/vim-buftabline'
    " Only show buffer line if there are > 2 buffers open.
    let g:buftabline_show=1
    let g:buftabline_numbers=2
endif
" {]} Litemode only

" {[}--- Misc ---
" Needed, really, because vim folding sucks otherwise.
Plug 'https://github.com/Konfekt/FastFold'
Plug 'https://github.com/Konfekt/FoldText'

Plug 'https://github.com/freitass/todo.txt-vim'

" Allows plugin maps to use '.' to repeat
Plug 'https://github.com/tpope/vim-repeat'

" Jump to specified place in file with file(L:C)
Plug 'https://github.com/wsdjeg/vim-fetch'
" For switching between header and alt files
" Plug 'vim-scripts/a.vim'
" if v:version >= 800 || has("patch-7.4.1829")
if has("timers")
    " Commands sent to shell with AsyncRun appear in qf window.
    " use AsyncRun! to prevent autoscroll.
    Plug 'https://github.com/skywind3000/asyncrun.vim'
    let g:hasAsyncrun = 1
    " Open quickfix window at height 8 on running
    let g:asyncrun_open = 8
    " cmap !! AsyncRun
    " cmap ! AsyncRun
    cabbrev ! AsyncRun
    let g:asyncrun_auto = "make"
    command! -bang -nargs=* -complete=file Make AsyncRun -program=make @ <args>
    " Set qf statusbar to status of asyncrun
    let g:asyncrun_status = "stopped"
    autocmd myPlugins QuickFixCmdPost * call asyncrun#quickfix_toggle(8, 1)
    autocmd myPlugins BufWinEnter quickfix setlocal 
                \ statusline=%t\ [%{g:asyncrun_status}]\ %{exists('w:quickfix_title')?\ '\ '.w:quickfix_title\ :\ ''}\ %=%-15(%l,%c%V%)\ %P
else
    let g:hasAsyncrun = 0
endif
" Confirms opening empty file on tabcomplete
Plug 'https://github.com/EinfachToll/DidYouMean'
" Close buffers without changing window
Plug 'https://github.com/moll/vim-bbye', {'on': 'Bdelete'}
cabbrev bd Bdelete

Plug 'https://github.com/chrisbra/csv.vim', {'for': 'csv'}
let g:csv_autocmd_arrange	   = 1
let g:csv_autocmd_arrange_size = 1024*1024
" let g:csv_highlight_column = 'y' " Current cursor's column.
" hi CSVColumnEven term=bold ctermbg=Gray guibg=LightGray
" TODO link to something so this doesn't look awful outside solarized light.
call add (g:customHLGroups, "CSVColumnEven guibg=gray90 ctermbg=lightgray")
" TODO Check if csv column highlight should be a highlight update or a
" pluginSettings update.
autocmd myPlugins User pluginSettingsToExec highlight clear CSVColumnOdd

" Needs nvim > 0.4, which was probably also when UIEnter was introduced.
if has('nvim') && exists('##UIEnter')
  exec 'source ' . g:plugindir . "/firenvim.vim"
endif

" {]} Misc

" {[}--- Yanks ---
Plug 'machakann/vim-highlightedyank'
if !exists('##TextYankPost')
    map y <Plug>(highlightedyank)
endif
" -1 gives persistent highlight until edit or new yank.
let g:highlightedyank_highlight_duration = 5000
" Needs unite/denite, no mappings by default.
" Maybe later on, put in ide and don't load yankring if idemode.
" if exists('##TextYankPost')
"     Plug 'Shougo/neoyank.vim'
"     let g:neoyank#file = &directory . 'yankring.txt'
" nmap <leader>p :unite history/yank
" else
if exists('##TextYankPost')
    Plug 'https://github.com/svermeulen/vim-yoink'
    if has('nvim')
        let g:yoinkSavePersistently = 1  " Nvim only.
    endif
    let g:yoinkSwapClampAtEnds = 0  " Cycle back to start.
    let g:yoinkSyncSystemClipboardOnFocus = 0

    nmap <expr> <leader>p yoink#canSwap() ?
                \ '<plug>(YoinkPostPasteSwapForward)' :
                \ '<plug>(YoinkPaste_p)'
    nmap <expr> <leader>P yoink#canSwap() ?
                \ '<plug>(YoinkPostPasteSwapBack)' :
                \ '<plug>(YoinkPaste_P)'

else
    Plug 'https://github.com/maxbrunsfeld/vim-yankstack.git'
    let g:yankstack_yank_keys = ['c', 'C', 'd', 'D', 'x', 'X', 'y', 'Y']
    autocmd myPlugins User pluginSettingsToExec call yankstack#setup()
    nmap <leader>p <Plug>yankstack_substitute_older_paste
    nmap <leader>P <Plug>yankstack_substitute_newer_paste
endif
" {]}--- Yanks ---

" {[}--- Operators ---
" Bunch of neat mappings, it's a tpope. Esp [n and ]n, for SCM conflict marks.
" And [<space> for addign newlines.
Plug 'https://github.com/tpope/vim-unimpaired'
" I want to use ]e (normally this) for errors.
nmap <silent> ]m <Plug>unimpairedMoveDown
nmap <silent> [m <Plug>unimpairedMoveUp
 let g:nremap = {"[e": "", "]e": ""}
 let g:xremap = {"[e": "", "]e": ""}
 let g:oremap = {"[e": "", "]e": ""}
" nunmap <silent> ]e
" nunmap <silent> [e
" cx to select an object, then cx again to swap it with first thing.
Plug 'https://github.com/tommcdo/vim-exchange'
Plug 'https://github.com/inkarkat/vim-ReplaceWithRegister'
nmap cr  <Plug>ReplaceWithRegisterOperator
nmap crr <Plug>ReplaceWithRegisterLine
" Causes delay on yank op.
" xmap yr  <Plug>ReplaceWithRegisterVisual
xmap R  <Plug>ReplaceWithRegisterVisual
" {]}--- Operators ---

" {[}--- Visual changes ---
if v:version >= 702
    " Highlight f and t chars to get where you want.
    " TODO monitor progress of this branch. May be updated soon.
    " Plug 'unblevable/quick-scope'
    Plug 'https://github.com/bradford-smith94/quick-scope'
    " Trigger a highlight in the appropriate direction when pressing these keys:
    let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
endif
if v:version >= 703
    Plug 'https://github.com/ntpeters/vim-better-whitespace'
    let g:better_whitespace_filetypes_blacklist=['diff', 'gitcommit', 'unite', 'qf', 'help', 'far']
    let g:show_spaces_that_precede_tabs=1
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
" Distraction-free vim.
Plug 'https://github.com/junegunn/goyo.vim', {'on' : ['Goyo',]}
" {]}--- Visual ---

" {[} --- TMUX ---
Plug 'https://github.com/tmux-plugins/vim-tmux'
Plug 'https://github.com/christoomey/vim-tmux-navigator'
Plug 'https://github.com/benmills/vimux'
" Prompt for a command to run
map <Leader>vp :VimuxPromptCommand<CR>
" Run last command executed by VimuxRunCommand
map <Leader>vl :VimuxRunLastCommand<CR>
" {]} TMUX

" {[} ---------- Prose ----------
Plug 'https://github.com/reedes/vim-pencil'
let g:pencil#wrapModeDefault = 'soft'
let g:pencil#conceallevel=&conceallevel
let g:pencil#concealcursor=&concealcursor
" Scratch isn't explicitly a prose ft, but for quick notes I want it treated
" as one.
autocmd myPlugins VimEnter * if &filetype == "scratch" | call pencil#init()
" {]} ---------- Prose----------
