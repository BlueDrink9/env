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

" {[} ---------- Misc ----------
if has('nvim-0.5')
    " Prerequisite for many nvim lua plugins.
    Plug 'nvim-lua/plenary.nvim'
endif

" Custom text for folds, includes indent level. Integrates with fastfold.
Plug 'https://github.com/Konfekt/FoldText'
if v:version > 704
    " Auto-set foldcolumn if folds exist in buffer.
    Plug 'https://github.com/benknoble/vim-auto-origami'
endif
" Relative line numbers only in focussed buffer & not in insert mode.
Plug 'ericbn/vim-relativize'
" :GhostTextStart/Stop
if has('nvim') && has('python3')
    Plug 'raghur/vim-ghost', {'do': ':GhostInstall', 'on': 'GhostStart'}
elseif has("channel") && has('python')
    " :GhostStart/Stop
    Plug 'atkenny15/vim-ghosttext', {'on': 'GhostStart'}
endif

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
if has('nvim')
    Plug 'https://github.com/numToStr/Comment.nvim'
else
    Plug 'https://github.com/tomtom/tcomment_vim'
endif


Plug 'jeetsukumaran/vim-markology', {'on': ['MarkologyEnable', 'MarkologyToggle']}
" https://github.com/chentoast/marks.nvim
"
" Adds a bunch of unix-mapped filesystem ops from vim
Plug 'https://github.com/tpope/vim-eunuch'
Plug 'https://github.com/simnalamburt/vim-mundo'

" Way better search and replace, also case coersion
Plug 'https://github.com/tpope/vim-abolish'
if v:version < 800 && !has('nvim')
    " Autoset Paste/nopaste
    Plug 'https://github.com/ConradIrwin/vim-bracketed-paste'
endif
if has('nvim')
    Plug 'https://github.com/monaqa/dial.nvim'
else
    Plug 'https://github.com/tpope/vim-speeddating'
    Plug 'https://github.com/Konfekt/vim-CtrlXA'
endif
" Fewer defaults, but preserves case and can apparently support latex.
" Plug 'https://github.com/bootleq/vim-cycle'
" map <silent> <Plug>CycleFallbackNext <Plug>SpeedDatingUp
" map <silent> <Plug>CycleFallbackPrev <Plug>SpeedDatingDown

if has('nvim')
    Plug 'echasnovski/mini.align'
else
    Plug 'https://github.com/junegunn/vim-easy-align'
endif

if v:version >= 703
    " Vim hexedit. Low dependency, interface as you'd expect. Pretty
    " good.
    Plug 'https://github.com/Shougo/vinarise.vim'
elseif executable('xxd')
    " Doesn't use autoload.
    Plug 'https://github.com/fidian/hexmodee', {'on': 'HexEdit'}
endif
if HasPython() && executable('pfp')
    " Powerful hexedit interface. Needs pip install pfp. Allows seing the
    " assembly.
    " Also requires downloading a .bt file for the filetype you are interested in.
    " Get at https://www.sweetscape.com/010editor/repository/templates/
    Plug 'https://github.com/d0c-s4vage/pfp-vim', {'on': 'HexEditFull'}
endif

" Limelight Looks really nice, esp for prose. Highlight slightly current paragraph.
Plug 'junegunn/limelight.vim', { 'for': g:proseFileTypes, 'on': 'Limelight' }
if has('nvim')
    Plug 'https://github.com/NMAC427/guess-indent.nvim'
else
    " Alternative is vim-sleuth, which seems overzealous.
    " This plugin has several forks. I am using the most updated one, but may be
    " worth playing with them.
    " Has a number of settings related to getting better accuracy.
    Plug 'https://github.com/idbrii/detectindent'
endif

" Zoom window to tab, and out again
Plug 'https://github.com/troydm/zoomwintab.vim'
" fzf for all the shortcuts defined in vim
Plug 'https://github.com/sunaku/vim-shortcut'
" {]} ---------- Misc----------

" {[} ---------- Visual changes ----------
if has('nvim-0.7')
    Plug 'https://github.com/stevearc/dressing.nvim'
endif
if !has('nvim-0.5')
    " nvim-0.5 has this built-in
    Plug 'machakann/vim-highlightedyank'
endif
" Resizes splits proportionally when changing overall size
Plug 'https://github.com/vim-scripts/ProportionalResize'
" {]} ---------- Visual changes ----------

" {[} View and session
" Automated view session creation.
Plug 'https://github.com/zhimsel/vim-stay', {'on': []}
call LoadPluginOnEvent('vim-stay', "CursorHold")
Plug 'xolox/vim-misc'
" Map os commands (eg maximise), and open windows commands without shell
" popup.
Plug 'https://github.com/xolox/vim-shell'
if v:version >= 704
    Plug 'https://github.com/xolox/vim-session'
endif
" {]} View and session

" {[} Extra text objects
" Additional text objects for next braket, i/a comma, pairs, smarter searching.
Plug 'wellle/targets.vim'
" Move args with >, <,. Next arg ], [,. New text obj a, i,.
" ],
Plug 'https://github.com/PeterRincker/vim-argumentative'
" Library for some other text obj plugins.
Plug 'https://github.com/kana/vim-textobj-user'
" See https://github.com/kana/vim-textobj-user/wiki for more, esp for
" lang-specific.
" Add text object for whole buffer
Plug 'https://github.com/kana/vim-textobj-entire'
" Expands what a sentence/word is for prose.
Plug 'https://github.com/reedes/vim-textobj-sentence', { 'for': g:proseFileTypes }
" av/iv for lines continued by \
Plug 'rhysd/vim-textobj-continuous-line'
" iz az
Plug 'somini/vim-textobj-fold'
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
Plug 'mtdl9/vim-log-highlighting'
Plug 'https://github.com/glts/vim-texlog'
" {]} Extra text objects

" {[} ---------- Operators ----------
" Replacement for surround, with more features.
Plug 'machakann/vim-sandwich'
if has('nvim-0.5')
    " Plug 'https://github.com/phaazon/hop.nvim'
    Plug 'https://github.com/ggandor/leap.nvim'
    Plug 'https://github.com/ggandor/leap-spooky.nvim'
    Plug 'https://github.com/ggandor/leap-ast.nvim'
else
    Plug 'https://github.com/justinmk/vim-sneak'
    " Plug 'https://github.com/easymotion/vim-easymotion'
endif
Plug 'bkad/camelcasemotion'
" {]} ---------- Operators ----------

" {[}--- Searching, replacing, finding ---
if has('timers')
    " Async, uses better grep tools like ack or ag
    Plug 'mhinz/vim-grepper', { 'on': ['Grepper', '<plug>(GrepperOperator)'] }
    " Live results, fuzzy buffer. Hideous.
    Plug 'wsdjeg/FlyGrep.vim'
    " Multi-file find and replace with a 'nice' interface. :Farp
    " I think this also needs python3
    " x - exclude. i - include. t - toggle. Capital X I T for all.
    Plug 'brooth/far.vim'

else
    " Bsgrep for searching in all open buffers. Also Bsreplace, Bstoc.
    Plug 'https://github.com/jeetsukumaran/vim-buffersaurus'

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
if !g:hasGUI || has('terminal') || has('nvim')
    " PlugInstall and PlugUpdate will clone fzf in ~/.fzf and run the install
    " script
    Plug 'junegunn/fzf', { 'dir': '~/.local/packages/fzf', 'do': './install --all' }
    " Adds some vim-specific fzf commands.
    Plug 'junegunn/fzf.vim'
else
    Plug 'https://github.com/ctrlpvim/ctrlp.vim'
endif
" {]}--- Fuzzy finder ---
" {]}--- Searching, replacing, finding ---

" {[} Tags
if has('timers')
    Plug 'https://github.com/wsdjeg/vim-todo'
else
    " :TaskList to show list of TODOs etc.
    Plug 'https://github.com/vim-scripts/TaskList.vim', {'on': ['Tasklist']}
endif
" {]}

"{[} Running/executing
" Run shell commands async (uses python)
" Plug 'https://github.com/joonty/vim-do'
Plug 'https://github.com/thinca/vim-quickrun'
" Select code to execute.
" Plug 'https://github.com/JarrodCTaylor/vim-shell-executor'
" Async make, autoset compiler and makeprg from filetype plugin (view quickfix with :COpen)
Plug 'https://github.com/tpope/vim-dispatch'
Plug 'https://github.com/radenling/vim-dispatch-neovim'
" Not sure how this compares to Dispatch. Not a complete replacement.
" Plug 'https://github.com/neomake/neomake'
" command! -bang -nargs=* -complete=file Make Neomake! <args>
"{]}

" {[} ---------- extra filetype support ----------
Plug 'kovetskiy/sxhkd-vim', {'for': 'sxhkdrc' }
" Adds syntax highlighting.
Plug 'vim-scripts/autohotkey-ahk', {'for': 'autohotkey'}
" Adds indent. Included in polyglot.
Plug 'https://github.com/hnamikaw/vim-autohotkey', {'for': 'autohotkey'}
Plug 'https://github.com/PProvost/vim-ps1', {'for': 'ps1'}
Plug 'https://github.com/lervag/vim-rmarkdown', {'for': 'rmd'}
Plug 'https://github.com/liuchengxu/graphviz.vim', {'for': 'dot'}
Plug 'https://github.com/waycrate/swhkd-vim', {'for': 'swhkd'}
if has("nvim")
    Plug 'https://github.com/nvim-orgmode/orgmode', {'for': 'org'}
    Plug 'https://github.com/salkin-mada/openscad.nvim', {'for': 'scad'}
else
    Plug 'https://github.com/jceb/vim-orgmode', {'for': 'org'}
endif

if has('nvim')
    Plug 'https://github.com/Nguyen-Hoang-Nam/nvim-preview-csv'
endif
Plug 'https://github.com/chrisbra/csv.vim', {'for': 'csv'}


" {]} ---------- extra filetype support ----------

" {[} ---------- Git ----------
if executable("git")
    " Git wrapper. Includes magit-style functionality under Gstatus
    Plug 'https://github.com/tpope/vim-fugitive'
    " Enhances working with branches in fugitive
    Plug 'sodapopcan/vim-twiggy'
    " TODO fugitive mapping to bb?
    " github wrapper
    " if v:version > 701
    "     Plug 'https://github.com/tpope/vim-rhubarb'
    " endif
    " Commit browser. :GV
    Plug 'junegunn/gv.vim'


    if has('nvim-0.7')
        " Can stage and all sorts.
        Plug 'https://github.com/lewis6991/gitsigns.nvim'
    else
        " VCS changes shown in sign column.
        Plug 'https://github.com/mhinz/vim-signify'
    endif

    " Plug 'airblade/vim-gitgutter'
    Plug 'https://github.com/christoomey/vim-conflicted', {'on': 'Conflicted'}
endif
" {]} ---------- Git----------

" {[} ---------- Prose ----------

" Better prose spellchecking
Plug 'https://github.com/reedes/vim-lexical', { 'for': g:proseFileTypes }

" Alternative to pencil, but modular if you want it.
" Plug 'https://github.com/vim-pandoc/vim-pandoc', { 'for': g:proseFileTypes }
" Plug 'https://github.com/vim-pandoc/vim-pandoc-syntax', { 'for': g:proseFileTypes }
" Plug 'https://github.com/vim-pandoc/vim-rmarkdown', {'for': 'rmd' }
" Pencil loaded in lite, for scratch.
" Plug 'https://github.com/reedes/vim-pencil'

Plug 'https://github.com/lervag/vimtex'

" {[} ---------- Markdown Preview ----------
if (has('nvim') || v:version >= 801) && !has('win32')
    " Downloads and uses a pre-build.
    Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': 'markdown'}
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
endif
" endif
" {]} ---------- Markdown Preview ----------

Plug 'https://github.com/dkarter/bullets.vim'
Plug 'ferrine/md-img-paste.vim', {'for': 'markdown'}
" {]} ---------- Prose----------

" {[} ---------- Terminal ----------
if has('nvim')
    Plug 'https://github.com/akinsho/toggleterm.nvim'
else
    " Convenient hide/show term buffer, $drop to open file with vim
    Plug 'skywind3000/vim-terminal-help'
endif
" {]} ---------- Terminal ----------

" {[} ---------- NerdTree Project/file drawer ----------
" Better version with more async and smarts.
" if has('python3') && has('nvim')
" Plug 'ms-jpq/chadtree'
" endif
" NT loads quite a lot in plugin/, so prefer to lazy-load when it gets opened.
" Downside is that netrw will be used instead when opening directories before NT opened for the first time.
Plug 'https://github.com/scrooloose/nerdtree.git', {'on': ['NERDTree', 'NERDTreeToggle',]}
Plug 'https://github.com/Xuyuanp/nerdtree-git-plugin'
" on windows, gvim is super slow with dirvish
if has('nvim') || !has('win32')
    Plug 'https://github.com/justinmk/vim-dirvish'
    Plug 'roginfarrer/vim-dirvish-dovish', {'branch': 'main'}
    Plug 'https://github.com/bounceme/remote-viewer'
    if executable('git')
        Plug 'https://github.com/kristijanhusak/vim-dirvish-git'
    endif
endif
" {]} ---------- NerdTree ----------

" {[} Neovim UIs/integrations
if has('nvim') && exists('##UIEnter')
    if exists('g:vscode')
    else
        " if has('win32')
        "     let s:firenvim_startup_prologue='"set LITE_SYSTEM=1"'
        " else
        "     let s:firenvim_startup_prologue='"export LITE_SYSTEM=1"'
        " endif
        let s:firenvim_startup_prologue=''
        let g:firenvim_install=":call firenvim#install(0, " . s:firenvim_startup_prologue . ")"
        " Only on tags/releases, because updates may require the extension to be
        " updated.
        Plug 'https://github.com/glacambre/firenvim', {'tag': '*', 'do': g:firenvim_install}
    endif
endif
" {]} Neovim UIs/integrations
