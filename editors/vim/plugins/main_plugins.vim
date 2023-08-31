" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" Maybe later, once I want them.
" {[} ---------- Later ----------
" Plugin 'https://github.com/dhruvasagar/vim-table-mode'
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
"     Plugin 'https://github.com/jalvesaq/zotcite'
" endif
" Use colourcolumn highlight for all of inactive windowsy.
" https://github.com/blueyed/vim-diminactive/
" {]} ---------- Later ----------

" {[} ---------- Providers/External model setup neovim ----------
let g:skipPythonInstall=1  " Tmp skip installing python modules.
" Install python module, preferably for py3.
function! PythonInstallModule(module)
    if exists('g:skipPythonInstall')
        return
    endif
    if Executable('conda')
        exec "!conda install -y -c conda-forge -c malramsay " . a:module
    else
        exec 'pyx import subprocess; import sys; subprocess.check_call([sys.executable,"-m","pip","install","' . a:module . '"])'
    endif
endfunction

" Should pick up from either python types.
if has('nvim') && !(has("python") || has("python3"))
    " Needed for neovim python support.
    call PythonInstallModule('neovim')
endif
" {]} ---------- Module setup ----------

" {[} ---------- Misc ----------
" Close buffers without changing window
Plugin 'https://github.com/moll/vim-bbye', {'on': 'Bdelete'}
cabbrev bd Bdelete
if has("timers")
    " Commands sent to shell with AsyncRun appear in qf window.
    " use AsyncRun! to prevent autoscroll.
    Plugin 'https://github.com/skywind3000/asyncrun.vim', {
                \ 'on': ['AsyncRun'], 'event': ['QuickFixCmdPost']}
endif
if has('nvim-0.5')
    " Prerequisite for many nvim lua plugins.
    Plugin 'nvim-lua/plenary.nvim'
endif

" Custom text for folds, includes indent level. Integrates with fastfold.
Plugin 'https://github.com/Konfekt/FoldText',
            \ has('nvim') ? {'event': ['VeryLazy']} : {}
if v:version > 704
    " Auto-set foldcolumn if folds exist in buffer.
    Plugin 'https://github.com/benknoble/vim-auto-origami', {'on': 'AutoOrigamiFoldColumn'}
endif
" Relative line numbers only in focussed buffer & not in insert mode.
Plugin 'ericbn/vim-relativize',
            \ has('nvim') ? {'event': ['VeryLazy']} : {}
" :GhostTextStart/Stop
if has('nvim') && has('python3')
    Plugin 'raghur/vim-ghost', {'do': ':GhostInstall', 'on': 'GhostStart'}
elseif has("channel") && has('python')
    " :GhostStart/Stop
    Plugin 'atkenny15/vim-ghosttext', {'on': 'GhostStart'}
endif

if v:version >= 704
    " Useful for overviews, and deleting lots of buffers.
    Plugin 'https://github.com/jlanzarotta/bufexplorer.git', {'on': 'BufExplorer'}
endif

if !has('nvim')
    Plugin 'https://github.com/tomtom/tcomment_vim'
endif


Plugin 'jeetsukumaran/vim-markology', {'on': ['MarkologyEnable', 'MarkologyToggle']}
" https://github.com/chentoast/marks.nvim
"
" Adds a bunch of unix-mapped filesystem ops from vim
Plugin 'https://github.com/tpope/vim-eunuch', 
            \ has('nvim') ? {'event': ['VeryLazy']} : {}
Plugin 'https://github.com/simnalamburt/vim-mundo', {'on': 'MundoToggle'}

" Way better search and replace, also case coersion
Plugin 'https://github.com/tpope/vim-abolish', {'keys': ['cr'],
            \ 'on': ['Abolish', 'Subvert', 'S']}
if v:version < 800 && !has('nvim')
    " Autoset Paste/nopaste
    Plugin 'https://github.com/ConradIrwin/vim-bracketed-paste'
endif
if has('nvim')
    Plugin 'https://github.com/monaqa/dial.nvim', {'keys':
                \ ['<C-a>', '<C-x>', 'g<C-a>', 'g<C-x>']}
else
    Plugin 'https://github.com/tpope/vim-speeddating'
    Plugin 'https://github.com/Konfekt/vim-CtrlXA'
endif
" Fewer defaults, but preserves case and can apparently support latex.
" Plugin 'https://github.com/bootleq/vim-cycle'
" map <silent> <Plug>CycleFallbackNext <Plug>SpeedDatingUp
" map <silent> <Plug>CycleFallbackPrev <Plug>SpeedDatingDown

if !has('nvim')
    Plugin 'https://github.com/junegunn/vim-easy-align'
endif

if v:version >= 703
    " Vim hexedit. Low dependency, interface as you'd expect. Pretty
    " good.
    Plugin 'https://github.com/Shougo/vinarise.vim'
elseif Executable('xxd')
    " Doesn't use autoload.
    Plugin 'https://github.com/fidian/hexmodee', {'on': 'HexEdit'}
endif
if HasPython() && Executable('pfp')
    " Powerful hexedit interface. Needs pip install pfp. Allows seing the
    " assembly.
    " Also requires downloading a .bt file for the filetype you are interested in.
    " Get at https://www.sweetscape.com/010editor/repository/templates/
    Plugin 'https://github.com/d0c-s4vage/pfp-vim', {'on': 'HexEditFull'}
endif

" Limelight Looks really nice, esp for prose. Highlight slightly current paragraph.
Plugin 'junegunn/limelight.vim', { 'for': g:proseFileTypes, 'on': 'Limelight' }
if !has('nvim')
    " Alternative is vim-sleuth, which seems overzealous.
    " This plugin has several forks. I am using the most updated one, but may be
    " worth playing with them.
    " Has a number of settings related to getting better accuracy.
    Plugin 'https://github.com/idbrii/detectindent'
endif

" Zoom window to tab, and out again
Plugin 'https://github.com/troydm/zoomwintab.vim', {'on': 'ZoomWinTabToggle'}
" fzf for all the shortcuts defined in vim
Plugin 'https://github.com/sunaku/vim-shortcut', {'on': 'Shortcut'}
" Confirms opening empty file on tabcomplete
Plugin 'https://github.com/EinfachToll/DidYouMean', {'event': ['BufReadPre']}
" {]} ---------- Misc----------

" {[} ---------- Visual changes ----------
if !has('nvim-0.5')
    " nvim-0.5 has this built-in
    Plugin 'machakann/vim-highlightedyank'
endif
" Resizes splits proportionally when changing overall size
Plugin 'https://github.com/vim-scripts/ProportionalResize',
            \ has('nvim') ? {'event': ['VeryLazy']} : {}
" {]} ---------- Visual changes ----------

" {[} View and session
" Automated view session creation.
Plugin 'https://github.com/zhimsel/vim-stay', {'on': [], 'event': ['CursorHold']}
" Map os commands (eg maximise), and open windows commands without shell
" popup.
let g:vim_shell_plug_args = {'dependencies': ['xolox/vim-misc']}
if has('nvim')
    let g:vim_shell_plug_args['event'] = 'VeryLazy'
endif
Plugin 'https://github.com/xolox/vim-shell', g:vim_shell_plug_args
if v:version >= 704
    Plugin 'https://github.com/xolox/vim-session', {'on': ['OpenSession'],
            \ 'dependencies': ['xolox/vim-misc']}
endif
" {]} View and session

" {[} Extra text objects
" Additional text objects for next bracket, i/a comma, pairs, smarter searching.
Plugin 'wellle/targets.vim', {
            \ 'keys': MakeLazyKeys(
            \ ['[', ']', 'i,', 'a', 'I', 'A'],
            \ ['n', 'o', 'v']) }
" Move args with >, <,. Next arg ], [,. New text obj a, i,.
" ],
Plugin 'PeterRincker/vim-argumentative', {
            \ 'keys': MakeLazyKeys(
            \ ['<,', '>,', '[,', '],', 'i,', 'a,' ],
            \ ['n', 'o', 'v']) }
" See https://github.com/kana/vim-textobj-user/wiki for more, esp for
" lang-specific.
" Add text object for whole buffer
Plugin 'https://github.com/kana/vim-textobj-entire', {
            \ 'dependencies': ['kana/vim-textobj-user'],
            \ 'on': ['<Plug>(textobj-entire-a)', '<Plug>(textobj-entire-i)']}
" av/iv for lines continued by \
" Plugin 'rhysd/vim-textobj-continuous-line', {
"             \ 'keys': MakeLazyKeys(["il", "al"], ["v","o"]),
"             \ 'dependencies': ['kana/vim-textobj-user']}
" iv as object for camelcasemotion style
" Plugin 'https://github.com/Julian/vim-textobj-variable-segment', {
"             \ 'dependencies': ['kana/vim-textobj-user']}
            " \ 'keys': ['iv', 'av'],
" iz az
Plugin 'somini/vim-textobj-fold', {
            \ 'keys': MakeLazyKeys(['az', 'iz'], ["v","o"]),
            \ 'dependencies': ['kana/vim-textobj-user']}
if v:version >= 703
    " ac, ic, aC
    Plugin 'https://github.com/glts/vim-textobj-comment', {
                \ 'keys': MakeLazyKeys(['ac', 'ic', 'aC', 'iC'], ["v","o"]),
                \ 'dependencies': ['kana/vim-textobj-user']}
endif
" Expands what a sentence/word is for prose.
Plugin 'https://github.com/reedes/vim-textobj-sentence', { 'for': g:proseFileTypes,
            \ 'keys': [')', '('],
            \ 'dependencies': ['kana/vim-textobj-user']}
Plugin 'https://github.com/coachshea/vim-textobj-markdown', { 'for': 'markdown',
            \ 'dependencies': ['kana/vim-textobj-user']}
" Adds indent block as text object. ii , ai or aI
Plugin 'michaeljsmith/vim-indent-object', {
            \ 'keys': MakeLazyKeys(['ii', 'ai', 'aI', 'iI'], ["v","o"])}
" Adds [ ] mappins for -=+% indentation objects
Plugin 'https://github.com/jeetsukumaran/vim-indentwise', {
            \ 'keys': ['[', ']']}
" af, if for functions, ac, ic for classes. Also ]pf, [pc for movements.
Plugin 'https://github.com/bps/vim-textobj-python', {'for': 'python',
            \ 'dependencies': ['kana/vim-textobj-user']}

Plugin 'mtdl9/vim-log-highlighting', {'for': 'log'}
Plugin 'https://github.com/glts/vim-texlog', {'for': ['log', 'tex']}
" {]} Extra text objects

" {[} ---------- Operators ----------
" Replacement for surround, with more features.
function! Plug_after_vim_sandwich()
    " Gives it tpope-surround mappings.
    runtime macros/sandwich/keymap/surround.vim
    vmap s <Plug>(operator-sandwich-add)
endf
Plugin 'machakann/vim-sandwich', {'keys': ['ys', 'ds', 'cs'],
            \ 'afterLoad': v:true}
if !has('nvim')
    Plugin 'https://github.com/justinmk/vim-sneak'
    " Plugin 'https://github.com/easymotion/vim-easymotion'
endif
Plugin 'bkad/camelcasemotion', {'keys': '-'}
Plugin 'https://github.com/haya14busa/vim-asterisk', {'keys': ['*', 'z*']}
" {]} ---------- Operators ----------

" {[}--- Searching, replacing, finding ---
" Edit quickfix window and have the changes apply!
Plugin 'https://github.com/stefandtw/quickfix-reflector.vim', {'for': 'qf'}
if has('timers')
    " Async, uses better grep tools like ack or ag
    Plugin 'mhinz/vim-grepper', { 'on': ['Grepper'] }
    " Live results, fuzzy buffer. Hideous.
    Plugin 'wsdjeg/FlyGrep.vim', {'on': ['FlyGrep']}
    " Multi-file find and replace with a 'nice' interface. :Farp
    " I think this also needs python3
    " x - exclude. i - include. t - toggle. Capital X I T for all.
    Plugin 'brooth/far.vim', {'on': ['Farp']}

else
    " Bsgrep for searching in all open buffers. Also Bsreplace, Bstoc.
    Plugin 'https://github.com/jeetsukumaran/vim-buffersaurus'

    " Quick find and replace text object, repeatable with .
    " Clobbers s so would need to remap.
    " Plugin 'https://github.com/hauleth/sad.vim'
    " nmap <leader>s <Plug>(sad-change-forward)
    " nmap <leader>S <Plug>(sad-change-backward)
    " xmap <leader>s <Plug>(sad-change-forward)
    " xmap <leader>S <Plug>(sad-change-backward)
endif

if !(has("nvim") || has("patch-8.1.0271"))
    " Live substitute preview.
    Plugin 'https://github.com/markonm/traces.vim'
endif

" Maybe ide candidates...
" {[}--- Fuzzy finder ---
" fzf only works in terminal, use ctrlp otherwise
if !g:hasGUI || has('terminal') || has('nvim')
    " PlugInstall and PlugUpdate will clone fzf in ~/.fzf and run the install
    " script
    Plugin 'junegunn/fzf', {
                \ 'do': { -> fzf#install() },
                \ 'on': [
                \ 'FZF',
                \ 'Buffers',
                \ 'BLines',
                \ 'Lines',
                \ 'Tags',
                \ 'Commands',
                \ 'History'
                \ ]}
    " Adds some vim-specific fzf commands.
    Plugin 'junegunn/fzf.vim', {'dependencies': ['junegunn/fzf'],
                \ 'on': [
                \ 'FZF',
                \ 'Buffers',
                \ 'BLines',
                \ 'Lines',
                \ 'Tags',
                \ 'Commands',
                \ 'History'
                \ ]}
else
    Plugin 'https://github.com/ctrlpvim/ctrlp.vim'
endif
" {]}--- Fuzzy finder ---
" {]}--- Searching, replacing, finding ---

" {[} Tags
if has('timers')
    Plugin 'https://github.com/wsdjeg/vim-todo', {'on': 'OpenTodo'}
else
    " :TaskList to show list of TODOs etc.
    Plugin 'https://github.com/vim-scripts/TaskList.vim', {'on': ['Tasklist']}
endif
" {]}

"{[} Running/executing
" Run shell commands async (uses python)
" Plugin 'https://github.com/joonty/vim-do'
" Plugin 'https://github.com/thinca/vim-quickrun'
" Select code to execute.
" Plugin 'https://github.com/JarrodCTaylor/vim-shell-executor'
" Async make, autoset compiler and makeprg from filetype plugin (view quickfix with :COpen)
Plugin 'https://github.com/tpope/vim-dispatch', {'on': ['Make', 'Start', 'Spawn']}
Plugin 'https://github.com/radenling/vim-dispatch-neovim', {'on': ['Make', 'Start', 'Spawn']}
" Not sure how this compares to Dispatch. Not a complete replacement.
" Plugin 'https://github.com/neomake/neomake'
" command! -bang -nargs=* -complete=file Make Neomake! <args>
"{]}

" {[} ---------- extra filetype support ----------
" Check that whatever you need isn't in polyglot, first!
" Multi-lang support
let g:polyglot_disabled = ['autoindent', 'sensible', 'latex', 'markdown', ]
Plugin 'https://github.com/sheerun/vim-polyglot'
Plugin 'https://github.com/lervag/vim-rmarkdown', {'for': 'rmd'}
Plugin 'https://github.com/liuchengxu/graphviz.vim', {'for': 'dot'}
Plugin 'https://github.com/waycrate/swhkd-vim', {'for': 'swhkd'}
if !has("nvim")
    Plugin 'https://github.com/jceb/vim-orgmode', {'for': 'org'}
    Plugin 'https://github.com/sirtaj/vim-openscad', {'for': 'scad'}
endif
" {]} ---------- extra filetype support ----------

" {[} ---------- Git ----------
if Executable("git")
    " Git wrapper. Includes magit-style functionality under Gstatus
    " Don't lazy-load
    Plugin 'https://github.com/tpope/vim-fugitive'
    " Enhances working with branches in fugitive
    Plugin 'sodapopcan/vim-twiggy', {'for': 'fugitive'}
    " TODO fugitive mapping to bb?
    " github wrapper
    " if v:version > 701
    "     Plugin 'https://github.com/tpope/vim-rhubarb'
    " endif


    if !has('nvim-0.7')
        " VCS changes shown in sign column.
        Plugin 'https://github.com/mhinz/vim-signify'
    endif

    " Plugin 'airblade/vim-gitgutter'
    Plugin 'https://github.com/christoomey/vim-conflicted', {'on': 'Conflicted'}
endif
" {]} ---------- Git----------

" {[} ---------- Prose ----------

" Better prose spellchecking
Plugin 'https://github.com/reedes/vim-lexical', { 'for': g:proseFileTypes }

" Alternative to pencil, but modular if you want it.
" Plugin 'https://github.com/vim-pandoc/vim-pandoc', { 'for': g:proseFileTypes }
" Plugin 'https://github.com/vim-pandoc/vim-pandoc-syntax', { 'for': g:proseFileTypes }
" Plugin 'https://github.com/vim-pandoc/vim-rmarkdown', {'for': 'rmd' }
Plugin 'https://github.com/reedes/vim-pencil', {
            \ 'for': g:proseFileTypes, 'on': 'Pencil'}

Plugin 'https://github.com/lervag/vimtex', {'for': 'tex',
            \ 'beforeFunc': 'VimtexBefore'}

" {[} ---------- Markdown Preview ----------
if (has('nvim') || v:version >= 801) && !has('win32')
    " Downloads and uses a pre-build.
    Plugin 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': 'markdown'}
endif
" else
" if has('python')
" Requires manually setting open cmd, requires py2.
"     Plugin 'previm/previm'
"     let g:previm_open_cmd="fopen"
" endif
if Executable('pandoc') && !has('win32')
    " Actually works, fewer dependencies (pandoc, not node or yarn). Doesn't
    " have synced scrolling, hard to change browser.
    Plugin 'JamshedVesuna/vim-markdown-preview', {'for': 'markdown'}
endif
" endif
" {]} ---------- Markdown Preview ----------

Plugin 'https://github.com/dkarter/bullets.vim', {'for': g:proseFileTypes}
Plugin 'ferrine/md-img-paste.vim', {'for': 'markdown'}
" {]} ---------- Prose----------

" {[} ---------- Terminal ----------
if !has('nvim')
    " Convenient hide/show term buffer, $drop to open file with vim
    Plugin 'skywind3000/vim-terminal-help'
endif
" {]} ---------- Terminal ----------

" {[} ---------- NerdTree Project/file drawer ----------
" Better version with more async and smarts.
" if has('python3') && has('nvim')
" Plugin 'ms-jpq/chadtree'
" endif
" NT loads quite a lot in plugin/, so prefer to lazy-load when it gets opened.
" Downside is that netrw will be used instead when opening directories before NT opened for the first time.
Plugin 'https://github.com/scrooloose/nerdtree.git', {'on': ['NERDTree', 'NERDTreeToggle',]}
Plugin 'https://github.com/Xuyuanp/nerdtree-git-plugin', {'on': ['NERDTree', 'NERDTreeToggle',]}
" on windows, gvim is super slow with dirvish
if has('nvim') || !has('win32')
    Plugin 'https://github.com/justinmk/vim-dirvish', {'on': 'Dirvish'}
    Plugin 'roginfarrer/vim-dirvish-dovish', {'branch': 'main', 'on': 'Dirvish'}
    " Plugin 'https://github.com/bounceme/remote-viewer'
    if Executable('git')
        Plugin 'https://github.com/kristijanhusak/vim-dirvish-git', {'on': 'Dirvish'}
    endif
endif
" {]} ---------- NerdTree ----------

" {[} Neovim UIs/integrations
if has('nvim') && exists('##UIEnter')
    if exists('g:vscode')
    endif
endif
" {]} Neovim UIs/integrations
