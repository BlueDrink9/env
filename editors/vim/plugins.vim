" vim: set ft=vim:

" Folder in which current script resides:
let s:path = expand('<sfile>:p:h')
let s:pluginPath = CreateVimDir("/vimfiles/plugins")

call plug#begin(s:pluginPath)

" Maybe later, once I want them.
" s + 2 letters jumps to it (like 2 letter f or t, but vert)
" Plug 'https://github.com/justinmk/vim-sneak'
" Plug 'https://github.com/easymotion/vim-easymotion'
" Allows plugin maps to use '.' to repeat
" Plug 'https://github.com/tpope/vim-repeat'
" Way better search and replace
" Plug 'https://github.com/tpope/vim-abolish'
" Uses leader rather than g
" Plug 'https://github.com/scrooloose/nerdcommenter'
" Awesome code completion, but requires specific installations
" Plug 'https://github.com/Valloric/YouCompleteMe'
" Align CSV files at commas, align Markdown tables, and more
" Plug 'godlygeek/tabular'

Plug 'https://github.com/altercation/vim-colors-solarized.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'https://github.com/tpope/vim-fugitive'
Plug 'https://github.com/maxbrunsfeld/vim-yankstack.git'
Plug 'https://github.com/scrooloose/nerdtree.git'
Plug 'https://github.com/jlanzarotta/bufexplorer.git'
Plug 'https://github.com/vim-scripts/ShowMarks.git'
Plug 'https://github.com/vim-syntastic/syntastic.git'
Plug 'https://github.com/Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'
" Adds a bunch of unix-mapped filesystem ops from vim
Plug 'https://github.com/tpope/vim-eunuch'
Plug 'https://github.com/tpope/vim-commentary'
Plug 'https://github.com/kien/rainbow_parentheses.vim'
Plug 'https://github.com/simnalamburt/vim-mundo'

Plug 'https://github.com/honza/vim-snippets'
Plug 'https://github.com/garbas/vim-snipmate.git'
Plug 'https://github.com/tomtom/tlib_vim.git'
Plug 'https://github.com/MarcWeber/vim-addon-mw-utils.git'

Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'
Plug 'majutsushi/tagbar'

Plug 'https://github.com/vim-airline/vim-airline'
Plug 'https://github.com/vim-airline/vim-airline-themes'
" exec "Plug \'https://github.com/vim-airline/vim-airline-themes\', {\'rtp\' : \'autoload/airline/themes/". colorSch . ".vim\'}"
Plug 'https://github.com/ctrlpvim/ctrlp.vim'
Plug 'https://github.com/xolox/vim-session'
Plug 'https://github.com/lervag/vimtex'
" For switching between header and alt files
Plug 'vim-scripts/a.vim'
Plug 'https://github.com/reedes/vim-pencil'

" TODO: move this to windows-specific
Plug 'https://github.com/vim-scripts/autohotkey-ahk'
call plug#end()

exec 'colorscheme ' . colorSch

" May be needed if terminal doesn't support.
exec 'let g:' . colorSch . '_termcolors=256'

" We need this for plugins like Syntastic and vim-gitgutter which put symbols
" in the sign column.
highlight clear SignColumn

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
exec 'let g:airline_solarized_bg="' . backgroundColor . '"'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1
let g:airline#extensions#tabline#show_tabs = 0
let g:airline#extensions#tabline#buffer_min_count = 2
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

" Session settings
let g:session_persist_colors = 0
let g:session_persist_font = 0
let g:session_default_to_last = 'yes'
let g:session_autosave_periodic = 5
let g:session_autosave = 'yes'
