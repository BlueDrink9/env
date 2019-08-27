" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" Maybe later, once I want them.
" {[} ---------- Later ----------
" Plug 'https://github.com/dhruvasagar/vim-table-mode'
" Scrollwheel on mouse moves screen with cursor (more natural)
" https://github.com/reedes/vim-wheel
" Function argument movements
" Plug 'https://github.com/PeterRincker/vim-argumentative'
" Bunch of paste stuff, replacing, yankring stuff.
" https://github.com/svermeulen/vim-easyclip
" Inertial scrolling, easier to see jump movement.
" Plug "https://github.com/yuttie/comfortable-motion.vim"
" {]} ---------- Later ----------

" {[} ---------- Python setup ----------
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
            endif
            let g:pyInstaller=l:pipVersion . " install --user --upgrade "
        endif
    endif
    exec "!" . g:pyInstaller . a:module
endfunction

" Should pick up from either python types.
if has('nvim') && !(has("python") || has("python3"))
    " Needed for neovim python support.
    call PythonInstallModule('neovim')
endif
" {]} ---------- Python setup ----------

" {[} ---------- Misc ----------
" :GhostTextStart/Stop
if has('nvim') && has('python3')
    Plug 'raghur/vim-ghost', {'do': ':GhostInstall'}
elseif has("channel") && has('python')
    " :GhostStart/Stop
    Plug 'atkenny15/vim-ghosttext'
endif
" ga on char shows all representations, not just dec oct hex.
Plug 'https://github.com/tpope/vim-characterize'
Plug 'https://github.com/metalelf0/supertab' " Fork with a failing feature removed
let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabLongestEnhanced = 1
let g:SuperTabLongestEnhanced = 1
" List of omni completion option names in the order of precedence that they should be used if available
" let g:SuperTabContextTextOmniPrecedence = ['&completefunc', '&omnifunc']

" Lighter-weight, native completion engine. TODO sort
" Plug 'https://github.com/ajh17/VimCompletesMe'
augroup vcm
    au!
    autocmd bufenter * let b:vcm_tab_complete = 'tags'
    autocmd FileType vim let b:vcm_tab_complete = 'vim'
    autocmd FileType vim let b:vcm_tab_complete = 'omni'
augroup end

if v:version >= 704
    " Useful for overviews, and deleting lots of buffers.
    Plug 'https://github.com/jlanzarotta/bufexplorer.git'
endif
" Separate buffer lists for differetn windows
" Plug 'https://github.com/zefei/vim-wintabs'
Plug 'https://github.com/tomtom/tcomment_vim'
let g:tcomment_opleader1='<leader>c'
let g:tcomment#blank_lines=0
xmap <C-/>  :Tcomment<CR>
nmap <C-/>  :TcommentBlock<CR>
omap <C-/>  :Tcomment<CR>
" I thought this wasn't working, because nothing ever showed up.
" It turns out this is because you need to :DoShowMarks first.
Plug 'https://github.com/jacquesbh/vim-showmarks.git'
" This one's ugly. May need to be customized.
" Plug 'jeetsukumaran/vim-markology'
" Adds a bunch of unix-mapped filesystem ops from vim
Plug 'https://github.com/tpope/vim-eunuch'
Plug 'https://github.com/simnalamburt/vim-mundo'
cabbrev undo MundoToggle

" Way better search and replace, also case coersion
Plug 'https://github.com/tpope/vim-abolish'
" Improves incremental search to match everythign that it should.
Plug 'https://github.com/haya14busa/incsearch.vim'
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
if v:version < 800 && !has('nvim')
  " Autoset Paste/nopaste
  Plug 'https://github.com/ConradIrwin/vim-bracketed-paste'
endif
" Allows plugin maps to use '.' to repeat
Plug 'https://github.com/tpope/vim-repeat'
Plug 'https://github.com/tpope/vim-speeddating'
Plug 'https://github.com/Konfekt/vim-CtrlXA'
nmap <Plug>SpeedDatingFallbackUp   <Plug>(CtrlXA-CtrlA)
nmap <Plug>SpeedDatingFallbackDown <Plug>(CtrlXA-CtrlX)
" Fewer defaults, but preserves case and can apparently support latex.
" Plug 'https://github.com/bootleq/vim-cycle'
" map <silent> <Plug>CycleFallbackNext <Plug>SpeedDatingUp
" map <silent> <Plug>CycleFallbackPrev <Plug>SpeedDatingDown

" Align CSV files at commas, align Markdown tables, and more.
" Could go in prose... but maybe I'll use it more later.
Plug 'https://github.com/junegunn/vim-easy-align'
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
" Adds indent block as text object. ii , ai or aI
Plug 'michaeljsmith/vim-indent-object'
" Adds [ ] mappins for -=+% indentation objects
Plug 'https://github.com/jeetsukumaran/vim-indentwise'
" Additional text objects for next braket, i/a comma, pairs, smarter searching.
Plug 'wellle/targets.vim'
" {]} ---------- Misc----------

" {[} ---------- Operators ----------

" Let's give it a go then.
Plug 'https://github.com/easymotion/vim-easymotion'
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
let g:EasyMotion_startofline = 0 " keep cursor column when JK motion
Plug 'bkad/camelcasemotion'
call add(g:pluginSettingsToExec, "call camelcasemotion#CreateMotionMappings('<leader>m')")
" {]} ---------- Operators ----------

" Maybe ide candidates...
" Fuzzy finder
" fzf only works in terminal, use ctrlp otherwise
if g:hasGUI && !has('terminal')
    Plug 'https://github.com/ctrlpvim/ctrlp.vim'
    let g:ctrlp_cmd = 'CtrlPMixed'
    let g:ctrlp_map = '<leader>f'
    let g:ctrlp_cache_dir = CreateVimDir("ctrpCache") " Purge cache with f5 in buffer
    let g:ctrlp_clear_cache_on_exit = 0
    if ideMode == 1
        let g:ctrlp_extensions = ['tag', 'buffertag', 'rtscript']
    endif
else
    " PlugInstall and PlugUpdate will clone fzf in ~/.fzf and run the install
    " script
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
      " Both options are optional. You don't have to install fzf in ~/.fzf
      " and you don't have to run the install script if you use fzf only in
      " Vim.
    nnoremap <leader>f :FZF<CR>
    " {[} Use proper fzf colours in gvim
    if g:hasGUI
        let g:fzf_colors =
                    \ { 'fg':      ['fg', 'Normal'],
                    \ 'bg':      ['bg', 'Normal'],
                    \ 'hl':      ['fg', 'Comment'],
                    \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
                    \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
                    \ 'hl+':     ['fg', 'Statement'],
                    \ 'info':    ['fg', 'PreProc'],
                    \ 'border':  ['fg', 'Ignore'],
                    \ 'prompt':  ['fg', 'Conditional'],
                    \ 'pointer': ['fg', 'Exception'],
                    \ 'marker':  ['fg', 'Keyword'],
                    \ 'spinner': ['fg', 'Label'],
                    \ 'header':  ['fg', 'Comment'] }
    endif
    " {]}
endif
" Run shell commands async (uses python)
Plug 'https://github.com/joonty/vim-do'
Plug 'https://github.com/thinca/vim-quickrun'
" Select code to execute.
Plug 'https://github.com/JarrodCTaylor/vim-shell-executor'
" Make is run async (view quickfix with :COpen)
Plug 'https://github.com/tpope/vim-dispatch'

" {[} ---------- Git ----------
if executable("git")
    " augroup myGit
    "     au!
    " augroup end
    " :Magit to check all sorts of git stuff. Looks really cool. Capitals for 
    " commands, eg [S]tage-toggle, [CC]ommit.
    Plug 'jreybert/vimagit'
    " Git wrapper
    Plug 'https://github.com/tpope/vim-fugitive'
    " nnoremap <leader>gs :Gstatus<CR> cabbrev gs Gstatus
    cabbrev gs Gstatus
    cabbrev gw Gwrite
    cabbrev gc Gwrite <bar> Gcommit
    cabbrev gco Gcommit
    cabbrev gup Gcommit --amend --no-edit
    cabbrev gdf Gdiff
    Plug 'sodapopcan/vim-twiggy'

    " Async fugitive
    if g:hasAsyncrun
        call add(g:pluginSettingsToExec, "command! -bang -nargs=* -complete=file Make AsyncRun -program=make @ <args>
                    \if exists(':Make') == 2
                    \noautocmd Make
                    \else
                    \silent noautocmd make!
                    \redraw!
                    \return 'call fugitive#cwindow()'
                    \endif")
    endif
    " github wrapper
    if v:version > 701
        Plug 'https://github.com/tpope/vim-rhubarb'
    endif
    " Commit browser. :GV
    Plug 'junegunn/gv.vim'
    Plug 'https://github.com/Xuyuanp/nerdtree-git-plugin'

    " VCS changes shown in sign column.
    Plug 'https://github.com/mhinz/vim-signify'
    " Add VCS systems to this when needed. More will slow buffer loading.
    let g:signify_vcs_list = [ 'git' ]
    " Async, so shouldn't be too bad. Ignored if not async.
    " let g:signify_realtime = 1
    " Causes a write on cursorhold. PITA, so let's replace it.
    " call add(g:pluginSettingsToExec, "autocmd! signify CursorHold,CursorHoldI")
    if has('timers')
        augroup mysignify
            au!
            " This seems to be causing an annoying error :/
            " autocmd CursorHold,CursorHoldI,BufEnter,FocusGained call silent! sy#start()
            " autocmd WinEnter call silent! sy#start()
            autocmd User Fugitive silent! SignifyRefresh
        augroup end
    endif
    " let g:signify_update_on_focusgained = 1
    let g:signify_sign_change = '~'

    " Plug 'airblade/vim-gitgutter'
    " " Allows hlcolumn bg to match coloursch
    " call add(g:customHLGroups, "clear SignColumn")
    " " gitgutter needs grep to not output escap sequences.
    " " let g:gitgutter_grep = ''
    " let g:gitgutter_grep = 'grep --color=never'
    " let g:gitgutter_override_sign_column_highlight = 0
    " let g:gitgutter_escape_grep = 1
    " " Disable automatic update
    " autocmd! gitgutter CursorHold,CursorHoldI
    " " " Wait 2000 ms after typing finishes before updating (vim default 4000)
    " " set updatetime=2000
    " augroup ggutter
    "     au!
    "     au BufWritePost * :GitGutter
    " augroup end
    " " Speed issues
    " " plugin only runs on BufRead, BufWritePost and FileChangedShellPost, i.e. when you open or save a file.
    " let g:gitgutter_realtime = 0
    " let g:gitgutter_eager = 0

    Plug 'https://github.com/christoomey/vim-conflicted'
    set stl+=%{ConflictedVersion()}
endif
" {]} ---------- Git----------

" {[} ---------- Prose ----------

" {[} ---------- Vimtex ----------
Plug 'https://github.com/lervag/vimtex'
" call add(g:pluginSettingsToExec, "let g:vimtex_compiler_latexmk.build_dir = 'latexbuild'")
call add(g:pluginSettingsToExec, "let g:vimtex_compiler_progname = 'nvr'")
let g:vimtex_fold_enabled = 1
let g:vimtex_fold_manual = 1 " instead of expr folding. Speeds up.
" Omnifunc complete citations, labels, filenames, glossary
let g:vimtex_complete_enabled = 1
let g:vimtex_imaps_disabled = []
" augroup my_vimtex
"     autocmd!
"     autocmd Filetype *tex set foldmethod=expr
" augroup END
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
    let s:pythonUserBase = system("python3 -c 'import site; print(site.USER_BASE, end=\"\")'")
    let $PATH=$PATH . ":" . PathExpand(s:pythonUserBase . "/bin")
    if has('nvim') && !has('clientserver')
        if !executable('nvr')
            " Currently won't download with conda. Doesn't support 3.7.
            if exists('g:pyInstaller') && !g:pyInstaller =~ "conda"
                call PythonInstallModule("neovim-remote")
            endif
        endif
        if executable('nvr')
            let g:vimtex_compiler_progname="nvr"
        endif
    endif
endif

function! SetVimtexMappings()
    " Ensure clean doesn't immediately get overridden...
    nnoremap <buffer> <localleader>lc :VimtexStop<cr>:VimtexClean<cr>
    inoremap <buffer> <c-b> \textbf{}<left>
    inoremap <buffer> <c-e> \textit{}<left>
    inoremap <buffer> <c-`> \texttt{}<left>
endfunction
augroup myVimtex
    au!
    autocmd Filetype tex :call SetVimtexMappings()
augroup end
" {]} ---------- Vimtex ----------

" Plug 'https://github.com/plasticboy/vim-markdown'
" Better prose spellchecking
exec "Plug 'https://github.com/reedes/vim-lexical', { 'for': " . g:proseFileTypes . " }"
let g:lexical#spell_key = '<leader>ls'
let g:lexical#thesaurus_key = '<leader>lt'
let g:lexical#dictionary_key = '<leader>ld'
" Neccesary for next plugin
exec "Plug 'https://github.com/kana/vim-textobj-user', { 'for': " . g:proseFileTypes . " }"
" Expands what a sentence/word is for prose.
exec "Plug 'https://github.com/reedes/vim-textobj-sentence', { 'for': " . g:proseFileTypes . " }" 
" vimL word usage highlighter
exec "Plug 'https://github.com/reedes/vim-wordy', { 'for': " . g:proseFileTypes . " }"
exec "Plug 'bluedrink9/vim-highlight-gender', { 'for': " . g:proseFileTypes . " }"
exec "Plug 'https://github.com/vim-scripts/LanguageTool', { 'for': " . g:proseFileTypes . " }"
" An alternative to langtool:https://github.com/rhysd/vim-grammarous 
exec "Plug 'https://github.com/panozzaj/vim-autocorrect', { 'for': " . g:proseFileTypes . " }"
" Limelight Looks really nice, esp for prose. Highlight slightly current paraghraph.
exec "Plug 'junegunn/limelight.vim', { 'for': " . g:proseFileTypes . " }"

function! SetProseOptions()
    " Add dictionary completion. Requires setting 'dictionary' option.
    setlocal complete+=k
    " Is this actually running these functions though?
    " call AutoCorrect()
    " call textobj#sentence#init()
    call add (g:pluginSettingsToExec, "call AutoCorrect()")
    call add (g:pluginSettingsToExec, "call textobj#sentence#init()")
    " Default spelling lang is En, I want en_nz.
    if &spelllang == "en"
        " Custom lang not set.
        setl spell spl=en_nz
    endif
    call pencil#init()
    setl ai
endfunction

" if &filetype == "" || &filetype == "scratch"
"     call pencil#init()
" endif
" Bullets.vim
let g:bullets_enabled_file_types = [
            \ 'markdown',
            \ 'text',
            \ 'gitcommit',
            \ 'scratch'
            \]
" {]} ---------- Prose----------

" {[} ---------- NerdTree ----------
Plug 'https://github.com/scrooloose/nerdtree.git'
" Change these if you feel the desire...
let g:NERDTreeIndicatorMapCustom = {
            \ "Modified"  : "✹",
            \ "Staged"    : "✚",
            \ "Untracked" : "?",
            \ "Renamed"   : "➜",
            \ "Unmerged"  : "═",
            \ "Deleted"   : "✖",
            \ "Dirty"     : "✗",
            \ "Clean"     : "✔︎",
            \ 'Ignored'   : '☒',
            \ "Unknown"   : "?"
            \}
" Open nerdtree in currently focussed window, rather than sidebar.
let NERDTreeHijackNetrw=1
" Delete buffer if delete file in NT.
let NERDTreeAutoDeleteBuffer = 1
let NERDTreeDirArrows = 1
let NERDTreeShowHidden=1
augroup NT
    autocmd!
    " Open nerdtree on directory edit (startup)
    autocmd StdinReadPre * let s:std_in=1
    autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif
    autocmd BufRead * if isdirectory(@%) | exec 'NERDTree' | endif
    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
augroup END
" {]} ---------- NerdTree ----------
