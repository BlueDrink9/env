" vim: set ft=vim:
" set laststatus=2

" Folder in which current script resides:
let s:scriptpath = fnameescape(expand('<sfile>:p:h'))
let s:vimfilesDir = CreateVimDir("vimfiles")
let s:pluginPath = CreateVimDir("vimfiles/plugins")
let s:localPlugins = fnameescape(expand(s:vimfilesDir . "/local_plugins.vim"))


" We need this for plugins like Syntastic and vim-gitgutter which put symbols
" in the sign column. Don't know if it should go before plugs or after
" colorscheme.
" highlight clear SignColumn
"
let g:proseFileTypes = "'tex,latex,context,plaintex,
            \markdown,mkd,
            \text,textile,
            \git,gitsendemail,
            \mail'"

if !filereadable(s:localPlugins)
    new
    silent exec 'write ' . s:localPlugins
    bdelete
endif

" To remove a Plugged repo using UnPlug
function! s:deregister(repo)
  let repo = substitute(a:repo, '[\/]\+$', '', '')
  let name = fnamemodify(repo, ':t:s?\.git$??')
  call remove(g:plugs, name)
  call remove(g:plugs_order, index(g:plugs_order, name))
endfunction
command! -nargs=1 -bar UnPlug call s:deregister(<args>)

call plug#begin(s:pluginPath)

" Get light plugin set first
exec 'source ' . s:scriptpath . "/light_plugins.vim"

" Maybe later, once I want them.
" s + 2 letters jumps to it (like 2 letter f or t, but vert)
" Plug 'https://github.com/justinmk/vim-sneak'
" Plug 'https://github.com/easymotion/vim-easymotion'
" Uses leader rather than g
" Plug 'https://github.com/scrooloose/nerdcommenter'
" Awesome code completion, but requires specific installations
" Plug 'https://github.com/Valloric/YouCompleteMe'
" Looks really nice, esp for prose. Highlight slightly current paraghraph.
" https://github.com/junegunn/limelight.vim
" Scrollwheel on mouse moves screen with cursor (more natural)
" https://github.com/reedes/vim-wheel
" Function argument movements
" Plug 'https://github.com/PeterRincker/vim-argumentative'

" NOTE: Remember that lightweight plugins (screen+resources+non-niche situations)
" go in light_plugins.vim

"--- Git ---"
Plug 'https://github.com/Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'
" Git wrapper
Plug 'https://github.com/tpope/vim-fugitive'
" github wrapper
Plug 'https://github.com/tpope/vim-rhubarb'
Plug 'https://github.com/christoomey/vim-conflicted'
set stl+=%{ConflictedVersion()}

"--- Snippits ---"
Plug 'https://github.com/honza/vim-snippets'
Plug 'https://github.com/garbas/vim-snipmate.git'
Plug 'https://github.com/tomtom/tlib_vim.git'
Plug 'https://github.com/MarcWeber/vim-addon-mw-utils.git'

"--- Tags ---"
Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'
Plug 'majutsushi/tagbar'

"--- Prose ---"
" Better prose spellchecking
            " Neccesary for next plugin
            " Expands what a sentence/word is for prose.
            " Plug 'https://github.com/plasticboy/vim-markdown'
exec "Plug 'https://github.com/reedes/vim-pencil'
            \| Plug 'https://github.com/reedes/vim-lexical', { 'for': " . g:proseFileTypes . " }
            \| Plug 'https://github.com/kana/vim-textobj-user'
            \| Plug 'https://github.com/reedes/vim-textobj-sentence'
            \| Plug 'https://github.com/tpope/vim-markdown'
            \| Plug 'https://github.com/reedes/vim-wordy', { 'for': " . g:proseFileTypes . " }
            \| Plug 'https://github.com/panozzaj/vim-autocorrect', { 'for': " . g:proseFileTypes . " }
            \"


Plug 'https://github.com/scrooloose/nerdtree.git'
Plug 'https://github.com/jacquesbh/vim-showmarks.git'
Plug 'https://github.com/vim-syntastic/syntastic.git'
" Adds a bunch of unix-mapped filesystem ops from vim
Plug 'https://github.com/tpope/vim-eunuch'
Plug 'https://github.com/simnalamburt/vim-mundo'
Plug 'https://github.com/fholgado/minibufexpl.vim'
Plug 'https://github.com/vim-airline/vim-airline'
Plug 'https://github.com/vim-airline/vim-airline-themes'
" exec "Plug \'https://github.com/vim-airline/vim-airline-themes\', {\'rtp\' : \'autoload/airline/themes/". colorSch . ".vim\'}"
Plug 'https://github.com/ctrlpvim/ctrlp.vim'
" For switching between header and alt files
Plug 'vim-scripts/a.vim'
Plug 'https://github.com/vim-latex/vim-latex'
let g:Tex_DefaultTargetFormat="pdf"
Plug 'https://github.com/lervag/vimtex'
" Run shell commands async (uses python)
Plug 'https://github.com/joonty/vim-do'
Plug 'https://github.com/python-mode/python-mode'
Plug 'https://github.com/thinca/vim-quickrun'
Plug 'https://github.com/vim-scripts/SingleCompile'
" Make is run async (view quickfix with :COpen)
Plug 'https://github.com/tpope/vim-dispatch'
" Way better search and replace, also case coersion
Plug 'https://github.com/tpope/vim-abolish'
Plug 'https://github.com/benmills/vimux'
" Autoset Paste/nopaste
Plug 'https://github.com/ConradIrwin/vim-bracketed-paste'
" Allows plugin maps to use '.' to repeat
Plug 'https://github.com/tpope/vim-repeat'
" Adds indent block as text object. ii , ai or aI
Plug 'michaeljsmith/vim-indent-object'
Plug 'bkad/camelcasemotion'
Plug 'https://github.com/tpope/vim-speeddating'
" Align CSV files at commas, align Markdown tables, and more.
" Could go in prose... but maybe I'll use it more later.
Plug 'https://github.com/junegunn/vim-easy-align'


" Unplugs and replacements go here

" \unplugs
exec 'source ' . s:localPlugins
call plug#end()

exec 'colorscheme ' . colorSch

if !has("gui_running")
" if $TERM contains "-256color"
    " May be needed if terminal doesn't support.
    exec 'let g:' . colorSch . '_termcolors=256'
" endif
endif

" ----- scrooloose/syntastic settings -----
let g:syntastic_error_symbol = '✘'
let g:syntastic_warning_symbol = "▲"
augroup mySyntastic
    au!
    au FileType tex let b:syntastic_mode = "passive"
augroup END

" Change these if you feel the desire...
let g:NERDTreeIndicatorMapCustom = {
            \ "Modified"  : "✹",
            \ "Staged"    : "✚",
            \ "Untracked" : "✭",
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

" ----- majutsushi/tagbar settings -----
" Open/close tagbar with \b
nmap <silent> <leader>b :TagbarToggle<CR>
" Uncomment to open tagbar automatically whenever possible
"autocmd BufEnter * nested :call tagbar#autoopen(0)


" ----- Syntastic -----
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" let g:syntastic_python_checkers = ['pylint']

" ----- Airline -----
let g:airline_theme=colorSch
let g:airline#extensions#wordcount#enabled = 1
exec 'let g:airline_' . colorSch . '_bg="' . backgroundColor . '"'
exec 'let g:airline_base16_' . colorSch . '= 0'
exec 'let g:' . colorSch . '_base16 = 0'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1
let g:airline#extensions#tabline#show_tabs = 0
" let g:airline#extensions#tabline#buffer_min_count = 2
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#buffers_label = 'B'
let g:airline#extensions#tabline#alt_sep = 1
let g:airline#extensions#tabline# = 1
" let g:airline#extensions#tabline#show_tabs = 0
" let g:airline_symbols_ascii=1
" let g:airline_powerline_fonts = 0
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
" unicode symbols
let g:airline#extensions#tabline#left_sep = '▶'
let g:airline#extensions#tabline#right_sep = '◀'
let g:airline#extensions#tabline#left_sep_alt = '|'
let g:airline#extensions#tabline#right_sep_alt = '|'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '☰'
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.columnnr = '∥'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'PASTE'
let g:airline_symbols.whitespace = '☲'
" airline symbols
let g:airline_symbols.readonly = ''
" Skip gap between col symbol and number (custom section)
 call airline#parts#define_raw('linenr', g:airline_symbols.linenr . ' %l')
 call airline#parts#define_raw('columnnr', g:airline_symbols.columnnr . '%c')
" call airline#parts#define_accent('linenr', 'bold')
let g:airline_section_z = airline#section#create([
            \ 'linenr', 'maxlinenr',' ', 'columnnr'])
let g:airline_section_tabline = airline#section#create(['%{getcwd()}'])
let g:airline#extensions#hunks#non_zero_only = 1
let g:airline#extensions#whitespace#checks = []
" Disable mode shown in cmdline
set noshowmode

" ----- Session -----
let g:session_persist_colors = 0
let g:session_persist_font = 0
let g:session_default_to_last = 'yes'
let g:session_autosave_periodic = 10
let g:session_autosave = 'yes'
let g:session_autoload = 'yes'
let g:session_directory = CreateVimDir("vimfiles/sessions/")

" ----- Prose -----
let g:pencil#wrapModeDefault = 'soft'
let g:lexical#spell_key = '<leader>ls'
let g:lexical#thesaurus_key = '<leader>lt'
let g:lexical#dictionary_key = '<leader>ld'
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
    call AutoCorrect()
    call textobj#sentence#init()
    setl spell spl=en_us
    call pencil#init()
    setl ai
endfunction
augroup prose
    autocmd!
    exec 'autocmd Filetype ' . g:proseFileTypes . ' call SetProseOptions()'
    " Override default prose settings for some files:
    " autocmd Filetype git,gitsendemail,*commit*,*COMMIT*
    "\ call pencil#init({'wrap': 'hard', 'textwidth': 72})
    autocmd BufEnter * if &filetype == "" | call pencil#init()
augroup END
" Bullets.vim
let g:bullets_enabled_file_types = [
            \ 'markdown',
            \ 'text',
            \ 'gitcommit',
            \ 'scratch'
            \]

"Better-whitespace
let g:show_spaces_that_precede_tabs=1
let g:better_whitespace_skip_empty_lines=1

" ----------- TMUX --------------
" Prompt for a command to run
map <Leader>vp :VimuxPromptCommand<CR>
" Run last command executed by VimuxRunCommand
map <Leader>vl :VimuxRunLastCommand<CR>


" ----------- Misc --------------

let g:ctrlp_cmd = 'CtrlPMixed'

call camelcasemotion#CreateMotionMappings('<leader>c')

let g:yankstack_yank_keys = ['c', 'C', 'd', 'D', 'x', 'X', 'y', 'Y']
call yankstack#setup()
nnoremap Y y$

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

nnoremap <leader>gc :Gwrite <bar> Gcommit<CR>
noremap <leader>gs :Gstatus<CR>
