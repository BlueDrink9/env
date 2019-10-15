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
    " Use better 'vim-sandwich' in main.
    Plug 'https://github.com/tpope/vim-surround.git'
" Plug 'machakann/vim-sandwich'
" " Gives it tpope-surround mappings.
" call add(pluginSettingsToExec, "runtime macros/sandwich/keymap/surround.vim")

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

if has('nvim') && has('node')
    " plugin firenvim in chrome and firefox.
    " Open textframes in nvim, similar to wasavi.
    Plug 'https://github.com/glacambre/firenvim', {'do': ':call firenvim#install(0)'}
    " Configured as json, basically.
    " Enable only on a few websites by default
    let g:firenvim_config = {
                \ 'localSettings': {
                \ '.*': {
                \ 'selector': 'textarea',
                \ 'priority': 0,
                \ },
                \ 'github\.com': {
                \ 'selector': 'textarea, * [contenteditable="true"]',
                \ 'priority': 1,
                \ },
                \ 'google\.com': {
                \ 'priority': 0,
                \ },
                \ },
                \ }
                " \ 'kaggle\.com': { " Regular kaggle is just for console.
                " \ 'priority': 0,
                " \ },
    function! s:FirenvimSetup(channel)
        let l:ui = nvim_get_chan_info(a:channel)
        if has_key(l:ui, 'client') &&
                    \ has_key(l:ui.client, "name") &&
                    \ l:ui.client.name == "Firenvim"
          " We are in firenvim
            let g:hasGUI=1
            let g:loaded_airline = 1
            let g:liteMode = 1
            silent! AirlineToggle
            call SetGFN(12)
            call add(g:customHLGroups, "EndOfBuffer guifg=guibg")
            " set nonumber
            " set norelativenumber
            set cmdheight=1
            set laststatus=0
            set noshowmode
            set termguicolors
            set noruler
            set noshowcmd
            set shortmess=aWAFtI
            " colorscheme PaperColor
            colorscheme github
            au! myVimrc FocusLost,InsertLeave,BufLeave * ++nested call Autosave()
            autocmd myPlugins BufNewFile *.txt call s:FirenvimSetFT()
        endif
    endfunction

    function! s:FirenvimSetFT()
        let l:bufname=expand('%:t')
        if l:bufname =~ "github.com"
            colorscheme github
            set ft=markdown
        elseif l:bufname =~ "cocalc.com" || l:bufname =~ "kaggleusercontent.com"
            set ft=python
        elseif l:bufname =~ "localhost"
            " Assume Jupyter notebook.
            set ft=python
        elseif l:bufname =~ "reddit.com"
            set ft=markdown
        elseif l:bufname =~ "stackexchange.com" || l:bufname =~ "stackoverflow.com"
            set ft=markdown
        endif
    endfunction

    autocmd myPlugins UIEnter * call s:FirenvimSetup(deepcopy(v:event.chan))
endif

" {]} Misc

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

