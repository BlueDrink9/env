" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" Maybe later, once I want them.
" {[} ---------- Later ----------
" Plug 'https://github.com/dhruvasagar/vim-table-mode'
" Nasty to configure. Unite source for zotero or bibtex (via python lib) citations.
" https://github.com/rafaqz/citation.vim
" Scrollwheel on mouse moves screen with cursor (more natural)
" https://github.com/reedes/vim-wheel
" Powerful changing of textobjects. See also
" https://www.reddit.com/r/vim/comments/f8obyy/why_doesnt_the_s_command_accept_a_motion/fimqru6/
" https://github.com/svermeulen/vim-subversive
" if has('python3')
"     " Allows direct insertion of zotero citations.
"     " Requires zot version > 5
"     Plug 'https://github.com/jalvesaq/zotcite'
" endif
" {]} ---------- Later ----------

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
if has('nvim') && !(has("python") || has("python3"))
    " Needed for neovim python support.
    call PythonInstallModule('neovim')
endif
" {]} ---------- Module setup ----------

" {[} Neovim UIs/integrations
" Needs nvim > 0.4, which was probably also when UIEnter was introduced.
if has('nvim') && exists('##UIEnter')
    call SourcePluginFile("firenvim.vim")
    call SourcePluginFile("vscode-neovim.vim")
endif

" {]} Neovim UIs/integrations

" {[} ---------- Misc ----------

" Custom text for folds, includes indent level. Integrates with fastfold.
Plug 'https://github.com/Konfekt/FoldText'
if v:version > 704
    " Auto-set foldcolumn if folds exist in buffer.
    Plug 'https://github.com/benknoble/vim-auto-origami'
    au myPlugins CursorHold,BufWinEnter,WinEnter * AutoOrigamiFoldColumn
endif
" Relative line numbers only in focussed buffer & not in insert mode.
Plug 'ericbn/vim-relativize'
" Needs manual activation. :RainbowParen, :RainbowParen!
Plug 'https://github.com/junegunn/rainbow_parentheses.vim'
" :GhostTextStart/Stop
if has('nvim') && has('python3')
    Plug 'raghur/vim-ghost', {'do': ':GhostInstall', 'on': 'GhostStart'}
elseif has("channel") && has('python')
    " :GhostStart/Stop
    Plug 'atkenny15/vim-ghosttext', {'on': 'GhostStart'}
endif
" ga on char shows all representations, not just dec oct hex.
Plug 'https://github.com/tpope/vim-characterize'
" For whatever reason, supertab just isn't mapping anything.
" Plug 'https://github.com/metalelf0/supertab' " Fork with a failing feature removed
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

" Lighter-weight, native completion engine. TODO sort
" Plug 'https://github.com/ajh17/VimCompletesMe'
" autocmd myPlugins bufenter * let b:vcm_tab_complete = 'tags'
" autocmd myPlugins FileType vim let b:vcm_tab_complete = 'vim'
" autocmd myPlugins FileType vim let b:vcm_tab_complete = 'omni'

if v:version >= 704
    " Useful for overviews, and deleting lots of buffers.
    Plug 'https://github.com/jlanzarotta/bufexplorer.git'
endif
" Separate buffer lists for differetn windows
" Plug 'https://github.com/zefei/vim-wintabs'
Plug 'https://github.com/tomtom/tcomment_vim'
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
xmap <C-/>  :Tcomment<CR>
nmap <C-/>  :TcommentBlock<CR>
omap <C-/>  :Tcomment<CR>
" I thought this wasn't working, because nothing ever showed up.
" It turns out this is because you need to :DoShowMarks first.
" Plug 'https://github.com/jacquesbh/vim-showmarks.git', {'on': 'DoShowMarks'}
" More advanced version of showmarks. Lots of mappings, eg m]
Plug 'jeetsukumaran/vim-markology', {'on': ['MarkologyEnable', 'MarkologyToggle']}
" Enable with m!
let g:markology_enable=0
let g:markology_ignore_type="hpq"
let g:markology_include=
            \ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.'`^<>[]()\""
call add (g:customHLGroups, "link MarkologyHLl LineNr")
call add (g:customHLGroups, "link MarkologyHLu LineNr")
call add (g:customHLGroups, "link MarkologyHLo LineNr")
" call add (g:customHLGroups, "link MarkologyHLo LineNr")
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

if v:version >= 703
    " Vim hexedit. Low dependency, interface as you'd expect. Pretty
    " good.
    Plug 'https://github.com/Shougo/vinarise.vim'
    let g:vinarise_enable_auto_detect=1
    let g:vinarise_detect_large_file_size=0
    " dump with VinarisePluginDump
    " let g:vinarise_objdump_command="objdump"
    let g:vinarise_objdump_intel_assembly=1

    au myPlugins FileType vinarise call s:setupVinarise()
    function s:setupVinarise()
        command! Assembly VinarisePluginDump
        command! Dump VinarisePluginDump
        DisableWhitespace
    endfunction
elseif executable('xxd')
    " Doesn't use autoload.
    Plug 'https://github.com/fidian/hexmodee', {'on': 'HexEdit'}
    command! HexEdit :HexMode<CR>
    let g:hexmode_autodetect = 1
    let g:hexmode_patterns = '*.bin,*.exe,*.dat,*.o'
endif
if HasPython() && executable('pfp')
    " Powerful hexedit interface. Needs pip install pfp. Allows seing the
    " assembly.
    " Also requires downloading a .bt file for the filetype you are interested in.
    " Get at https://www.sweetscape.com/010editor/repository/templates/
    Plug 'https://github.com/d0c-s4vage/pfp-vim', {'on': 'HexEditFull'}
    command! HexEditFull :PfpParse<CR>
endif

" Limelight Looks really nice, esp for prose. Highlight slightly current paragraph.
Plug 'junegunn/limelight.vim', { 'for': g:proseFileTypes, 'on': 'Limelight' }
" Alternative is vim-sleuth, which seems overzealous.
" This plugin has several forks. I am using the most updated one, but may be
" worth playing with them.
" Has a number of settings related to getting better accuracy.
Plug 'https://github.com/idbrii/detectindent'
let g:detectindent_preferred_expandtab = &expandtab
let g:detectindent_preferred_indent = &shiftwidth
au myPlugins BufReadPost * silent! :DetectIndent

" Zoom window to tab, and out again
Plug 'https://github.com/troydm/zoomwintab.vim'
nnoremap <c-w>z :ZoomWinTabToggle<cr>
" fzf for all the shortcuts defined in vim
Plug 'https://github.com/sunaku/vim-shortcut'
" {]} ---------- Misc----------

" {[} ---------- Visual changes ----------
Plug 'machakann/vim-highlightedyank'
if !exists('##TextYankPost')
    map y <Plug>(highlightedyank)
endif
" -1 gives persistent highlight until edit or new yank.
let g:highlightedyank_highlight_duration = 5000
" Resizes splits proportionally when changing overall size
Plug 'https://github.com/vim-scripts/ProportionalResize'
" {]} ---------- Visual changes ----------

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
    let g:session_directory = CreateVimDir("sessions")
    cabbrev cs CloseSession
    cabbrev os OpenSession
    cabbrev ss SaveSession
endif
" {]} View and session

" {[} Extra text objects
" iv = current viewable text in the buffer
onoremap iv :exec "normal! HVL"<cr>
" Additional text objects for next braket, i/a comma, pairs, smarter searching.
Plug 'wellle/targets.vim'
" Don't handle argument. Use another plugin
autocmd User targets#mappings#user call targets#mappings#extend({
            \ 'a': {},
            \ })
" Move args with >, <,. Next arg ], [,. New text obj a, i,.
" ],
Plug 'https://github.com/PeterRincker/vim-argumentative'
" Library for some other text obj plugins.
Plug 'https://github.com/kana/vim-textobj-user'
" See https://github.com/kana/vim-textobj-user/wiki for more, esp for
" lang-specific.
" Expands what a sentence/word is for prose.
Plug 'https://github.com/reedes/vim-textobj-sentence', { 'for': g:proseFileTypes }
" Called by vim-plug when this is loaded.
autocmd! User vim-textobj-sentence call textobj#sentence#init()
" Adds il, al. Alternatively, '_' is the official object for the current line.
Plug 'kana/vim-textobj-line'
Plug 'kana/vim-textobj-entire'
let g:textobj_entire_no_default_key_mappings=1
omap a% <Plug>(textobj-entire-a)
vmap a% <Plug>(textobj-entire-a)
omap i% <Plug>(textobj-entire-i)
vmap i% <Plug>(textobj-entire-i)
" av/iv for lines continued by \
Plug 'rhysd/vim-textobj-continuous-line'
" iz az
Plug 'somini/vim-textobj-fold'
Plug 'lucapette/vim-textobj-underscore'
if v:version >= 703
    " ac, ic, aC
    Plug 'https://github.com/glts/vim-textobj-comment'
endif
Plug 'https://github.com/coachshea/vim-textobj-markdown', { 'for': 'markdown' }
" Function argument movements
Plug 'https://github.com/PeterRincker/vim-argumentative'
" Adds indent block as text object. ii , ai or aI
Plug 'michaeljsmith/vim-indent-object'
" Adds [ ] mappins for -=+% indentation objects
Plug 'https://github.com/jeetsukumaran/vim-indentwise'
" af, if for functions, ac, ic for classes. Also ]pf, [pc for movements.
Plug 'https://github.com/bps/vim-textobj-python'
" {]} Extra text objects

" {[} ---------- Operators ----------
" Replacement for surround, with more features.
Plug 'machakann/vim-sandwich'
" Gives it tpope-surround mappings.
autocmd myPlugins User pluginSettingsToExec runtime macros/sandwich/keymap/surround.vim
Plug 'https://github.com/easymotion/vim-easymotion'
let g:EasyMotion_do_mapping = 0 " Disable default mappings
" tab-incrementable search with easymotion dropout feature.
" map  <leader>/ <Plug>(easymotion-sn)
" omap <leader>/ <Plug>(easymotion-tn)
let g:EasyMotion_smartcase = 1
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
let g:EasyMotion_startofline = 0 " keep cursor column when JK motion
" Cross window boundaries
nmap <Leader><S-K> <Plug>(easymotion-overwin-line)
nmap <Leader><S-J> <Plug>(easymotion-overwin-line)
nmap <Leader><S-L> <Plug>(easymotion-overwin-w)
nmap <Leader><S-H> <Plug>(easymotion-overwin-w)
" Like sneak
nnoremap <leader>s <Plug>(easymotion-overwin-f2)
Plug 'bkad/camelcasemotion'
autocmd myPlugins User pluginSettingsToExec call camelcasemotion#CreateMotionMappings('<leader>m')
" {]} ---------- Operators ----------

" {[}--- Searching, replacing, finding ---
if has('timers')
    " Async, uses better grep tools like ack or ag
    Plug 'mhinz/vim-grepper', { 'on': ['Grepper', '<plug>(GrepperOperator)'] }
    let g:grepper = {
                \ 'tools': ['rg', 'ag', 'ack', 'findstr', 'pt', 'git', 'grep'],
                \ }
    cabbrev bfind Grepper -query

    " Live results, fuzzy buffer. Hideous.
    Plug 'wsdjeg/FlyGrep.vim'
    cabbrev bsearch FlyGrep
    nnoremap <leader>/ :FlyGrep<CR>
    let g:FlyGrep_input_delay = 200  " ms. default 500

    " Multi-file find and replace with a 'nice' interface. :Farp
    " I think this also needs python3
    " x - exclude. i - include. t - toggle. Capital X I T for all.
    Plug 'brooth/far.vim'
    command! Replace Farp
    " Project replace.
    " nnoremap <leader>pr :Farp<CR>
    function! s:farMappings()
        nnoremap <buffer><silent> q :call g:far#close_preview_window()<cr>
        nnoremap <buffer><silent> <bs> :call g:far#change_collapse_under_cursor(-1)<cr>
        nnoremap <buffer><silent> <c-CR> :Fardo<CR>
        nnoremap <buffer><silent> W :Refar<CR>
        nnoremap <buffer><silent> r :Fardo<CR>
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

else
    " Bsgrep for searching in all open buffers. Also Bsreplace, Bstoc.
    Plug 'https://github.com/jeetsukumaran/vim-buffersaurus'
    cabbrev bfind Bsgrep
    command! Find Bsgrep
    command! Replace2 Bsreplace
    let g:buffersaurus_autodismiss_on_select=0

    " Quick find and replace text object, repeatable with .
    " Clobbers s so would need to remap.
    " Plug 'https://github.com/hauleth/sad.vim'
    " nmap <leader>s <Plug>(sad-change-forward)
    " nmap <leader>S <Plug>(sad-change-backward)
    " xmap <leader>s <Plug>(sad-change-forward)
    " xmap <leader>S <Plug>(sad-change-backward)
endif

if !(has("nvim") || has("patch-8.1.0271"))
    " Live substitute preview.
    Plug 'https://github.com/markonm/traces.vim'
endif

" Maybe ide candidates...
" {[}--- Fuzzy finder ---
" fzf only works in terminal, use ctrlp otherwise
if g:hasGUI && !has('terminal')
    Plug 'https://github.com/ctrlpvim/ctrlp.vim'
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
else
    call SourcePluginFile("fzf.vim")
endif
" {]}--- Fuzzy finder ---
" {]}--- Searching, replacing, finding ---

" {[} Tags
if has('timers')
    Plug 'https://github.com/wsdjeg/vim-todo'
    command! Todo OpenTodo
else
    " :TaskList to show list of TODOs etc.
    Plug 'https://github.com/vim-scripts/TaskList.vim', {'on': ['Tasklist']}
    " Need a mapping to prevent it auto-creating and overriding <leader>t
    nmap <Plug>__blank <Plug>TaskList
    command! Todo TaskList
endif
" {]}

"{[} Running/executing
" Run shell commands async (uses python)
Plug 'https://github.com/joonty/vim-do'
Plug 'https://github.com/thinca/vim-quickrun'
" Select code to execute.
Plug 'https://github.com/JarrodCTaylor/vim-shell-executor'
" Make is run async (view quickfix with :COpen)
Plug 'https://github.com/tpope/vim-dispatch'
"{]}

" {[} ---------- Basic extra filetype support ----------
Plug 'kovetskiy/sxhkd-vim', {'for': 'sxhkd' }
" Adds syntax highlighting.
Plug 'vim-scripts/autohotkey-ahk', {'for': 'autohotkey'}
" Adds indent. Included in polyglot.
Plug 'https://github.com/hnamikaw/vim-autohotkey', {'for': 'autohotkey'}
Plug 'https://github.com/PProvost/vim-ps1', {'for': 'ps1'}
Plug 'https://github.com/lervag/vim-rmarkdown', {'for': 'rmd'}
" {]} ---------- Basic extra filetype support ----------

" {[} ---------- Git ----------
if executable("git")
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
    cabbrev gupw Gwrite <bar> Gcommit --amend --no-edit
    cabbrev gup Gcommit --amend --no-edit
    cabbrev gupe Gcommit --amend
    cabbrev gdf Gdiff
    autocmd myPlugins filetype fugitive setlocal nobuflisted

    " Async fugitive
    if exists('g:hasAsyncrun') && g:hasAsyncrun
        autocmd myPlugins User pluginSettingsToExec command!
                    \ -bang -nargs=* -complete=file
                    \ Make AsyncRun -program=make @ <args>
                    \ if exists(':Make') == 2
                    \ noautocmd Make
                    \ else
                    \ silent noautocmd make!
                    \ redraw!
                    \ return 'call fugitive#cwindow()'
                    \ endif
    endif
    " Enhances working with branches in fugitive
    Plug 'sodapopcan/vim-twiggy'
    " github wrapper
    if v:version > 701
        Plug 'https://github.com/tpope/vim-rhubarb'
    endif
    " Commit browser. :GV
    Plug 'junegunn/gv.vim'

    " VCS changes shown in sign column.
    Plug 'https://github.com/mhinz/vim-signify'
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
    " au myPlugins BufWritePost * :GitGutter
    " " Speed issues
    " " plugin only runs on BufRead, BufWritePost and FileChangedShellPost, i.e. when you open or save a file.
    " let g:gitgutter_realtime = 0
    " let g:gitgutter_eager = 0

    Plug 'https://github.com/christoomey/vim-conflicted', {'on': 'Conflicted'}
    function! s:setupConflicted()
        " TODO: integrate with airline.
        set stl+=%{ConflictedVersion()}
        " Resolve and move to next conflicted file.
        nnoremap ]m :GitNextConflict<cr>
        nnoremap [m :GitPrevConflict<cr>
    endfunction
    let g:diffget_local_map = 'gl'
    let g:diffget_upstream_map = 'gu'
    autocmd myVimrc User VimConflicted call s:setupConflicted()
endif
" {]} ---------- Git----------

" {[} ---------- Prose ----------

" Better prose spellchecking
Plug 'https://github.com/reedes/vim-lexical', { 'for': g:proseFileTypes }
let g:lexical#spell_key = '<localleader>ls'
let g:lexical#thesaurus_key = '<localleader>lt'
let g:lexical#dictionary_key = '<localleader>ld'

" Alternative to pencil, but modular if you want it.
" Plug 'https://github.com/vim-pandoc/vim-pandoc', { 'for': g:proseFileTypes }
" Plug 'https://github.com/vim-pandoc/vim-pandoc-syntax', { 'for': g:proseFileTypes }
" Plug 'https://github.com/vim-pandoc/vim-rmarkdown', {'for': 'rmd' }
" Pencil loaded in lite, for scratch.
" Plug 'https://github.com/reedes/vim-pencil'
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

" TODO check that SetProseOptions is run at the right times.
function! SetProseOptions()
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

" {[} ---------- Vimtex ----------
Plug 'https://github.com/lervag/vimtex'
" autocmd myPlugins User pluginSettingsToExec exec "let g:vimtex_compiler_latexmk.build_dir = 'latexbuild'"
autocmd myPlugins User pluginSettingsToExec let g:vimtex_compiler_progname = 'nvr'
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
endfunction
" autocmd myPlugins Filetype *tex set foldmethod=expr
autocmd myPlugins Filetype tex :call SetVimtexMappings()
" {]} ---------- Vimtex ----------

" {[} ---------- Markdown Preview ----------
if (has('nvim') || v:version >= 801) && !has('win32')
    " Downloads and uses a pre-build.
    Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': 'markdown'}
    let g:mkdp_auto_start = 0
    let g:mkdp_auto_close = 0
    let g:mkdp_echo_preview_url = 1
    " On save, insertleave
    " let g:mkdp_refresh_slow = 1
endif
" else
" if has('python')
" Requires manually setting open cmd, requires py2.
"     Plug 'previm/previm'
"     let g:previm_open_cmd="fopen"
" endif
if executable('pandoc') && !has('win32')
    " Actually works, fewer dependencies (pandoc, not node or yarn). Doesn't
    " have synced scrolling, hard to change browser.
    Plug 'JamshedVesuna/vim-markdown-preview', {'for': 'markdown'}
    let vim_markdown_preview_toggle=2
    let vim_markdown_preview_hotkey='<localleader>r'
    command! MarkdownPreview :call Vim_Markdown_Preview()<CR>
    let vim_markdown_preview_pandoc=1
endif
" endif
" {]} ---------- Markdown Preview ----------

Plug 'https://github.com/dkarter/bullets.vim'
let g:bullets_enabled_file_types = [
            \ 'markdown',
            \ 'text',
            \ 'gitcommit',
            \ 'scratch'
            \]
Plug 'ferrine/md-img-paste.vim', {'for': 'markdown'}
autocmd myPlugins FileType markdown command! PasteImage silent call mdip#MarkdownClipboardImage()<CR>
" {]} ---------- Prose----------

" {[} ---------- Terminal ----------
" REPL (send motions, lines etc)
Plug 'kassio/neoterm'
" Split instead of replacing buffer.
let g:neoterm_default_mod='belowright'
let g:neoterm_size=10  " Rows
" Autostart repl on command
let g:neoterm_auto_repl_cmd=1
" Don't use shell as intermediary, open REPL directly.
let g:neoterm_direct_open_repl=1
" Use tab neoterm instead of most recent.
let g:neoterm_term_per_tab=1
" start in insert mode
let g:neoterm_autoinsert=1
let g:neoterm_autoscroll=1
call Nmap(g:IDE_mappings.REPLSend, "<Plug>(neoterm-repl-send)")
call Vmap(g:IDE_mappings.REPLSend, "<Plug>(neoterm-repl-send)")
call Nmap(g:IDE_mappings.REPLSendLine, "<Plug>(neoterm-repl-send-line)")
call Nmap(g:IDE_mappings.REPLClear, ":Tclear<cr>")
call Nmap(g:IDE_mappings.REPLClose, ":Tclose<cr>")

" Useful for REPL, but can also send the commands back to the other window.
" Also dot repeatable.
" Have to specify direction to send, however.
" https://github.com/KKPMW/vim-sendtowindow
" Super lightweight, have to specify command for each filetype.
" Example config in readme.
" https://github.com/axvr/zepl.vim

" Convenient hide/show term buffer, $drop to open file with vim
Plug 'skywind3000/vim-terminal-help'
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
if has("win32")
    let g:terminal_shell="powershell"
endif
" {]} ---------- Terminal ----------

" {[} ---------- NerdTree ----------
Plug 'https://github.com/scrooloose/nerdtree.git', {'on': ['NERDTree', 'NERDTreeToggle',]}
Plug 'https://github.com/Xuyuanp/nerdtree-git-plugin'
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
" Open nerdtree on directory edit (startup)
autocmd myPlugins StdinReadPre * let s:std_in=1
autocmd myPlugins VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif
autocmd myPlugins BufRead * if isdirectory(@%) | exec 'NERDTree' | endif
autocmd myPlugins bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

cabbrev nt NERDTreeToggle
nnoremap _ :NERDTreeToggle<CR>
" {]} ---------- NerdTree ----------
