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
    nmap l<leader>c <Plug>ChangeCommentary
    nmap <leader>cu <Plug>Commentary<Plug>Commentary
    xmap <C-/>  <Plug>Commentary
    nmap <C-/>  <Plug>Commentary
    omap <C-/>  <Plug>Commentary
    " {]} Replace Tcomment with commentary

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
Plug 'https://github.com/moll/vim-bbye'
cabbrev bd Bdelete

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
Plug 'https://github.com/maxbrunsfeld/vim-yankstack.git'
let g:yankstack_yank_keys = ['c', 'C', 'd', 'D', 'x', 'X', 'y', 'Y']
call add(g:pluginSettingsToExec, "call yankstack#setup()")
nmap <leader>p <Plug>yankstack_substitute_older_paste
nmap <leader>P <Plug>yankstack_substitute_newer_paste
" {]}--- Yanks ---

Plug 'https://github.com/chrisbra/csv.vim'
let g:csv_autocmd_arrange	   = 1
let g:csv_autocmd_arrange_size = 1024*1024
" let g:csv_highlight_column = 'y' " Current cursor's column.
" hi CSVColumnEven term=bold ctermbg=Gray guibg=LightGray
" TODO link to something so this doesn't look awful outside solarized light.
call add (g:customHLGroups, "CSVColumnEven guibg=gray90 ctermbg=lightgray")
call add (g:pluginSettingsToExec, "highlight clear CSVColumnOdd")
" {]} Misc

" {[}--- Operators ---
" Replacement for surround, with more features.
Plug 'machakann/vim-sandwich'
" Gives it tpope-surround mappings.
call add(pluginSettingsToExec, "runtime macros/sandwich/keymap/surround.vim")
" Plug 'https://github.com/tpope/vim-surround.git'
" Bunch of neat mappings, it's a tpope. Esp [n and ]n, for SCM conflict marks.
" And [<space> for addign newlines.
Plug 'https://github.com/tpope/vim-unimpaired'
" Because ]e for ale_next clobbers this
nmap <silent> ]m <Plug>unimpairedMoveDown
nmap <silent> [m <Plug>unimpairedMoveUp
" cx to select an object, then cx again to swap it with first thing.
Plug 'https://github.com/tommcdo/vim-exchange'
" {]}--- Operators ---

" {[}--- Visual ---
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
    let g:show_spaces_that_precede_tabs=1
    let g:better_whitespace_skip_empty_lines=0
    " Don't HL current line. May cause performance issues.
    let g:current_line_whitespace_disabled_hard=0
    call add (g:customHLGroups, "ExtraWhitespace ctermbg=Gray guibg=LightGray")
    " call add (g:customHLGroups, "link ExtraWhitespace Visual")
endif
" Relative line numbers only in focussed buffer & not in insert mode.
Plug 'ericbn/vim-relativize'
" Needs manual activation. :RainbowParen, :RainbowParen!
Plug 'https://github.com/junegunn/rainbow_parentheses.vim'
" Distraction-free vim.
Plug 'https://github.com/junegunn/goyo.vim'
" {]}--- Visual ---

" {[}--- Searching, replacing ---
if has('timers')
    " Async, uses better grep tools like ack or ag
    Plug 'mhinz/vim-grepper', { 'on': ['Grepper', '<plug>(GrepperOperator)'] }
    let g:grepper = {
          \ 'tools': ['rg', 'ag', 'ack', 'findstr', 'pt', 'git', 'grep'],
          \ }
    cabbrev bfind Grepper -query

    " Multi-file find and replace with a nice interface. May be useful, idk.
    Plug 'brooth/far.vim'
else
    " Bsgrep for searching in all open buffers. Also Bsreplace, Bstoc.
    Plug 'https://github.com/jeetsukumaran/vim-buffersaurus'
    cabbrev bfind Bsgrep
    let g:buffersaurus_autodismiss_on_select=0

    " Quick find and replace text object, repeatable with .
    " Clobbers s so would need to remap.
    " Plug 'https://github.com/hauleth/sad.vim'
    " nmap <leader>s <Plug>(sad-change-forward)
    " nmap <leader>S <Plug>(sad-change-backward)
    " xmap <leader>s <Plug>(sad-change-forward)
    " xmap <leader>S <Plug>(sad-change-backward)
endif
" {]}--- Searching, replacing ---

" {[} --- TMUX ---
Plug 'https://github.com/tmux-plugins/vim-tmux'
Plug 'https://github.com/christoomey/vim-tmux-navigator'
Plug 'https://github.com/benmills/vimux'
" Prompt for a command to run
map <Leader>vp :VimuxPromptCommand<CR>
" Run last command executed by VimuxRunCommand
map <Leader>vl :VimuxRunLastCommand<CR>
" {]} TMUX

" {[} View and session
" Automated view session creation.
Plug 'https://github.com/zhimsel/vim-stay'
set viewoptions=cursor,folds,slash,unix
Plug 'xolox/vim-misc'
" Map os commands (eg maximise), and open windows commands without shell
" popup.
Plug 'https://github.com/xolox/vim-shell'
if v:version >= 704
    Plug 'https://github.com/xolox/vim-session'
    let g:session_persist_globals = ['&spelllang', '&autoread', '&spell']
    let g:session_persist_colors = 0
    let g:session_persist_font = 0
    " Open recent session instead of default
    let g:session_default_to_last = 'yes'
    let g:session_autosave_periodic = 10
    let g:session_autosave = 'yes'
    let g:session_autoload = 'no' " Could also be 'prompt'
    let g:session_directory = CreateVimDir(g:vimfilesDir . "/sessions/")
    cabbrev cs CloseSession
    cabbrev os OpenSession
    cabbrev ss SaveSession
endif
" {]} View and session


" {[} ---------- Prose ----------
" if has('nvim')
"     " Needs node, yarn.
"     Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() } }
"     let g:mkdp_auto_start = 0
"     let g:mkdp_auto_close = 0
"     " On save, insertleave
"     " let g:mkdp_refresh_slow = 0
" else
" if has('python')
" Requires manually setting open cmd, requires py2.
"     Plug 'previm/previm'
"     let g:previm_open_cmd="fopen"
" endif
if executable('pandoc')
    " Actually works, fewer dependencies (pandoc, not node or yarn). Doesn't
    " have synced scrolling, hard to change browser.
    Plug 'JamshedVesuna/vim-markdown-preview'
    let vim_markdown_preview_toggle=2
    let vim_markdown_preview_hotkey='<localleader>r'
    command! MarkdownPreview :call Vim_Markdown_Preview()<CR>
    let vim_markdown_preview_pandoc=1
endif
" endif
" Plug 'https://github.com/vim-latex/vim-latex'


" Alternative to pencil, but modular if you want it.
" Plug 'vim-pandoc/vim-pandoc'
Plug 'https://github.com/reedes/vim-pencil'
Plug 'https://github.com/dkarter/bullets.vim'
let g:pencil#wrapModeDefault = 'soft'
let g:pencil#conceallevel=&conceallevel
let g:pencil#concealcursor=&concealcursor
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

function! SetProseOptions()
    " Default spelling lang is En, I want en_nz.
    if &spelllang == "en"
        " Custom lang not set.
        setl spell spl=en_nz
    endif
    call pencil#init()
    setl ai
endfunction

exec 'autocmd myPlugins Filetype ' . g:proseFileTypes . ' call SetProseOptions()'
" Override default prose settings for some files:
" autocmd Filetype git,gitsendemail,*commit*,*COMMIT*
"\ call pencil#init({'wrap': 'hard', 'textwidth': 72})
autocmd myPlugins BufEnter * if &filetype == "" || &filetype == "scratch" | call pencil#init()
" {]} ---------- Prose----------
