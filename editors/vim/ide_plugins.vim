" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" {[} ---------- Linting ----------
if v:version >= 800
    " Async linting
    Plug 'https://github.com/w0rp/ale'
else
    " ----- syntastic -----
    Plug 'https://github.com/vim-syntastic/syntastic.git'
    let g:syntastic_cpp_compiler_options="-std=c++11"
    "  Airline handles status stuff (or should)
    " set statusline+=%#warningmsg#
    " set statusline+=%{SyntasticStatuslineFlag()}
    " set statusline+=%*
    " let g:syntastic_stl_format = "[%E{Err: #%e L%fe}%B{, }%W{Warn: #%w L%fw}]"

    let g:syntastic_enable_signs=1
    let g:syntastic_always_populate_loc_list = 1
    let g:syntastic_auto_loc_list = 1
    let g:syntastic_loc_list_height = 5
    " open errors when present, close when done.
    let g:syntastic_auto_loc_list = 1
    let g:syntastic_check_on_open = 1
    let g:syntastic_check_on_wq = 0
    " let g:syntastic_python_checkers = ['pylint']
    let g:syntastic_vim_checkers = ['vint']

    let g:syntastic_error_symbol = 'X'
    let g:syntastic_warning_symbol = "!"
    augroup mySyntastic
        autocmd!
        au FileType tex let b:syntastic_mode = "passive"
    augroup END
    " TODO make this window-specific.
    " let g:syntastic_shell = "/bin/sh"
endif
" {]} ---------- Linting----------

" {[} ---------- Completion ----------
" Awesome code completion, but requires specific installations
" Plug 'https://github.com/Valloric/YouCompleteMe'
" Plug 'jerrymarino/icompleteme'
if v:version >= 800
    " Async completion engine, doesn't need extra installation.
    Plug 'maralla/completor.vim'
    " Use TAB to complete when typing words, else inserts TABs as usual.  Uses
    " dictionary, source files, and completor to find matching words to complete.

    " Note: usual completion is on <C-n> but more trouble to press all the time.
    " Never type the same word twice and maybe learn a new spellings!
    " Use the Linux dictionary when spelling is in doubt.
    function! Tab_Or_Complete() abort
        " If completor is already open the `tab` cycles through suggested completions.
        if pumvisible()
            return "\<C-N>"
            " If completor is not open and we are in the middle of typing a word then
            " `tab` opens completor menu.
        elseif col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
            return "\<C-R>=completor#do('complete')\<CR>"
        else
            " If we aren't typing a word and we press `tab` simply do the normal `tab`
            " action.
            return "\<Tab>"
        endif
    endfunction

    " Use `tab` key to select completions.  Default is arrow keys.
    inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
    inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

    " Use tab to trigger auto completion.  Default suggests completions as you type.
    let g:completor_auto_trigger = 0
    inoremap <expr> <Tab> Tab_Or_Complete()
endif
" Python completion, plus some refactor, goto def and usage features.
Plug 'https://github.com/davidhalter/jedi-vim', { 'do' : 'AsyncRun pip install jedi' }
let g:jedi#use_splits_not_buffers = "right"
" {]} ---------- Completion----------

" {[} ---------- Tags ----------
Plug 'xolox/vim-misc'
if executable('ctags-exuberant') || executable('ctags')
    Plug 'xolox/vim-easytags'
    let g:easytags_async=1
    Plug 'majutsushi/tagbar'
    nmap <silent> <leader>tb :TagbarToggle<CR>
    augroup tag
        au!
        " Uncomment to open tagbar automatically whenever possible
        autocmd BufEnter * nested :call tagbar#autoopen(0)
    augroup end
endif
" {]} ---------- Tags----------

" {[} ---------- Snippits ----------
Plug 'https://github.com/honza/vim-snippets' " Library of snippets
if (has("python") || has("python3")) && v:version >= 704
    Plug 'https://github.com/SirVer/ultisnips' " Snippit engine
    let g:UltiSnipsExpandTrigger="<c-n>"
    let g:UltiSnipsJumpForwardTrigger="<c-n>"
    let g:UltiSnipsJumpBackwardTrigger="<c-p>"
    " Maybe use these to map <CR> to trigger in future?
   " autocmd! User UltiSnipsEnterFirstSnippet
   " autocmd User UltiSnipsEnterFirstSnippet call CustomInnerKeyMapper()
   " autocmd! User UltiSnipsExitLastSnippet
   " autocmd User UltiSnipsExitLastSnippet call CustomInnerKeyUnmapper()
else
    Plug 'https://github.com/tomtom/tlib_vim.git'
    Plug 'https://github.com/MarcWeber/vim-addon-mw-utils.git'
    Plug 'https://github.com/garbas/vim-snipmate'
    let g:snipMate = {}
    let g:snipMate['description_in_completion'] = 1
    let g:snipMate['no_match_completion_feedkeys_chars'] = ''
    imap <C-N> <Plug>snipMateNextOrTrigger
    smap <C-N> <Plug>snipMateNextOrTrigger
    imap <C-P> <Plug>snipMateBack
    smap <C-P> <Plug>snipMateBack
endif
" way smaller engine than ultisnips, not really much func. Can't use snip libs.
" Plug 'https://github.com/joereynolds/vim-minisnip'
" {]} ---------- Snippits----------

" {[} ---------- Syntax ----------
Plug 'https://github.com/python-mode/python-mode', { 'branch': 'develop' }
Plug 'octol/vim-cpp-enhanced-highlight'
let g:cpp_class_decl_highlight = 1
let g:cpp_member_variable_highlight = 1
Plug 'https://github.com/WolfgangMehner/c-support'
Plug 'https://github.com/dragfire/Improved-Syntax-Highlighting-Vim'
" For extensive cpp IDE stuff.
" a.vim incompat with replacement provided here.

" Plug 'https://github.com/LucHermitte/lh-dev'
" Plug 'https://github.com/LucHermitte/mu-template'
" Plug 'tomtom/stakeholders_vim.git'
" Plug 'https://github.com/LucHermitte/lh-brackets' " Ooooh boy this one's problematic.
" Plug 'https://github.com/LucHermitte/lh-vim-lib'
" Plug 'luchermitte/lh-cpp'
" May cause lag on scrolling.
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
" Multi-lang support
Plug 'https://github.com/sheerun/vim-polyglot'
" {]} ---------- Syntax----------

" {[} ---------- Git ----------
if executable("git")
    " Advanced commit history browser
    Plug 'https://github.com/junegunn/gv.vim'
    " Better diff algs with :PatientDiff or :EnhancedDiff
    Plug 'https://github.com/chrisbra/vim-diff-enhanced'
endif
" {]} ---------- Git----------

" {[} ---------- IDE ----------
Plug 'https://github.com/mh21/errormarker.vim'
let &errorformat="%f:%l:%c: %t%*[^:]:%m,%f:%l: %t%*[^:]:%m," . &errorformat
let errormarker_disablemappings = 1
cabbrev er ErrorAtCursor
Plug 'ryanoasis/vim-devicons'
if has("python3")
    Plug 'https://github.com/vim-vdebug/vdebug'
endif
" Look up documtenation for word under cursor with gk
Plug 'https://github.com/keith/investigate.vim'
" {]} ---------- IDE----------
" Quickly compile small files with :SCCompile
Plug 'https://github.com/xuhdev/SingleCompile'
" Customisable start screen, including MRU files
Plug 'https://github.com/mhinz/vim-startify'
let g:startify_session_dir = CreateVimDir("vimfiles/sessions/")
" visually show indentation
if v:version >= 703
    Plug 'https://github.com/Yggdroot/indentLine'
endif

" {[} ---------- Neosettings ----------
" https://github.com/kepbod/ivim/blob/master/vimrc
  " if g:ivim_autocomplete=='NEO'
  "       if has('lua')
  "           let g:ivim_completion_engine='neocomplete'
  "           Plug 'Shougo/neocomplete.vim' " Auto completion framework
   " let g:neocomplete#enable_at_startup=1
   "          let g:neocomplete#data_directory=$HOME . '/.vim/cache/neocomplete'
   "          let g:neocomplete#enable_auto_delimiter=1
   "          " Use <C-E> to close popup
   "          inoremap <expr><C-E> neocomplete#cancel_popup()
   "          inoremap <expr><CR> delimitMate#WithinEmptyPair() ?
   "                      \ "\<C-R>=delimitMate#ExpandReturn()\<CR>" :
   "                      \ pumvisible() ? neocomplete#close_popup() : "\<CR>""
" if !exists('g:neocomplete#force_omni_input_patterns')
"                 let g:neocomplete#force_omni_input_patterns={}
"             endif
"             let g:neocomplete#force_omni_input_patterns.python=
"             \ '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
  "       else
  "           let g:ivim_completion_engine='neocomplcache'
  "           Plug 'Shougo/neocomplcache.vim' " Auto completion framework
  " let g:neocomplcache_enable_at_startup=1
  "           let g:neocomplcache_temporary_dir=$HOME . '/.vim/cache/neocomplcache'
  "           let g:neocomplcache_enable_auto_delimiter=1
  "           let g:neocomplcache_enable_fuzzy_completion=1
  "           " Use <C-E> to close popup
  "           inoremap <expr><C-E> neocomplcache#cancel_popup()
  "           inoremap <expr><CR> delimitMate#WithinEmptyPair() ?
  "                       \ "\<C-R>=delimitMate#ExpandReturn()\<CR>" :
  "                       \ pumvisible() ? neocomplcache#close_popup() : "\<CR>""
" if !exists('g:neocomplcache_force_omni_patterns')
"                 let g:neocomplcache_force_omni_patterns={}
"             endif
"             let g:neocomplcache_force_omni_patterns.python=
"             \ '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
  " "       endif
  "       Plug 'Shougo/neosnippet.vim' " Snippet engine
  "       Plug 'Shougo/neosnippet-snippets' " Snippets
  "       Plug 'Shougo/vimproc.vim', { 'do': 'make' }
  "       Plug 'wellle/tmux-complete.vim' " Completion for tmux panes
    " " -> Neocomplete & Neocomplcache
    "     " Use Tab and S-Tab to select candidate
    "     inoremap <expr><Tab>  pumvisible() ? "\<C-N>" : "\<Tab>"
    "     inoremap <expr><S-Tab> pumvisible() ? "\<C-P>" : "\<S-Tab>""
  "   else
  "       " Auto completion framework
  "       let g:ivim_completion_engine='YouCompleteMe'
  "       Plug 'Valloric/YouCompleteMe', { 'do': './install.py' } "Auto completion framework
  "       Plug 'honza/vim-snippets' " Snippets
  "       Plug 'sirver/ultisnips' " Snippet engine
  "   endif
" {]} ---------- Neosettings----------
