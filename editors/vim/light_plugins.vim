" vim: foldmethod=marker
" vim: foldmarker={[},{]}
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

" {[}--- Misc ---
" Confirms opening empty file on tabcomplete
Plug 'https://github.com/EinfachToll/DidYouMean'
Plug 'https://github.com/Konfekt/FastFold'
Plug 'https://github.com/Konfekt/FoldText'
Plug 'https://github.com/wsdjeg/vim-fetch'
" Bunch of neat mappings, it's a tpope. Esp [n and ]n, for SCM conflict marks.
" And [<space> for addign newlines.
Plug 'https://github.com/tpope/vim-unimpaired'
" Because ]e for ale_next clobbers this
nmap <silent> ]m <Plug>unimpairedMoveDown
nmap <silent> [m <Plug>unimpairedMoveUp
Plug 'https://github.com/chrisbra/csv.vim'
let g:csv_autocmd_arrange	   = 1
let g:csv_autocmd_arrange_size = 1024*1024
" let g:csv_highlight_column = 'y' " Current cursor's column.
" hi CSVColumnEven term=bold ctermbg=Gray guibg=LightGray
call add (g:customHLGroups, "CSVColumnEven guibg=gray90 ctermbg=lightgray")
call add (g:pluginSettingsToExec, "highlight clear CSVColumnOdd")
" For switching between header and alt files
" Plug 'vim-scripts/a.vim'
if has('timers')
    " Async, uses better grep tools like ack or ag
    Plug 'mhinz/vim-grepper', { 'on': ['Grepper', '<plug>(GrepperOperator)'] }
    cabbrev bfind Grepper -query

    " Multi-file find and replace with a nice interface. May be useful, idk.
    Plug 'brooth/far.vim'
else
    " Bsgrep for searching in all open buffers. Also Bsreplace, Bstoc.
    Plug 'https://github.com/jeetsukumaran/vim-buffersaurus'
    cabbrev bfind Bsgrep
    let g:buffersaurus_autodismiss_on_select=0
endif
if v:version >= 703
    Plug 'https://github.com/ntpeters/vim-better-whitespace'
    let g:show_spaces_that_precede_tabs=1
    let g:better_whitespace_skip_empty_lines=1
    let g:better_whitespace_operator='_s'
    call add (g:customHLGroups, "ExtraWhitespace ctermbg=Gray guibg=LightGray")
    " call add (g:customHLGroups, "link ExtraWhitespace CursorColumn")
endif
" cx to select an object, then cx again to swap it with first thing.
Plug 'https://github.com/tommcdo/vim-exchange'
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
    augroup qfas
        au!
        autocmd QuickFixCmdPost * call asyncrun#quickfix_toggle(8, 1)
        autocmd BufWinEnter quickfix setlocal 
                    \ statusline=%t\ [%{g:asyncrun_status}]\ %{exists('w:quickfix_title')?\ '\ '.w:quickfix_title\ :\ ''}\ %=%-15(%l,%c%V%)\ %P
    augroup END
else
    let g:hasAsyncrun = 0
endif
" {]} Misc

" {[} --- TMUX ---
Plug 'https://github.com/tmux-plugins/vim-tmux'
Plug 'https://github.com/christoomey/vim-tmux-navigator'
Plug 'https://github.com/benmills/vimux'
" Prompt for a command to run
map <Leader>vp :VimuxPromptCommand<CR>
" Run last command executed by VimuxRunCommand
map <Leader>vl :VimuxRunLastCommand<CR>
" {]} TMUX

" Close buffers without changing window
Plug 'https://github.com/moll/vim-bbye'
cabbrev bd Bdelete
Plug 'ericbn/vim-relativize'
if v:version >= 702
    " Highlight f and t chars to get where you want.
    " TODO monitor progress of this branch. May be updated soon.
    " Plug 'unblevable/quick-scope'
    Plug 'https://github.com/bradford-smith94/quick-scope'
    " Trigger a highlight in the appropriate direction when pressing these keys:
    let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
endif

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
    let g:session_directory = CreateVimDir("vimfiles/sessions/")
    cabbrev cs CloseSession
    cabbrev os OpenSession
    cabbrev ss SaveSession
endif

Plug 'https://github.com/tpope/vim-surround.git'
Plug 'machakann/vim-highlightedyank'
if !exists('##TextYankPost')
    map y <Plug>(highlightedyank)
endif
" Persistent highlight until edit or new yank.
let g:highlightedyank_highlight_duration = -1

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
if v:version >= 704
    Plug 'https://github.com/jlanzarotta/bufexplorer.git'
endif
Plug 'https://github.com/junegunn/rainbow_parentheses.vim'
" Distraction-free vim.
Plug 'https://github.com/junegunn/goyo.vim'



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
let g:pencil#conceallevel=2
let g:pencil#concealcursor=""
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
    nnoremap <buffer> gk k
    nnoremap <buffer> gj j
    vnoremap <buffer> gk k
    vnoremap <buffer> gj j
    setl ai
endfunction

augroup prose
    autocmd!
    exec 'autocmd Filetype ' . g:proseFileTypes . ' call SetProseOptions()'
    " Override default prose settings for some files:
    " autocmd Filetype git,gitsendemail,*commit*,*COMMIT*
    "\ call pencil#init({'wrap': 'hard', 'textwidth': 72})
    autocmd BufEnter * if &filetype == "" || &filetype == "scratch" | call pencil#init()
augroup END
" {]} ---------- Prose----------
