" vim: set ft=vim:

" set laststatus=2

" Folder in which current script resides:
let s:scriptpath = fnameescape(expand('<sfile>:p:h'))
let s:vimfilesDir = CreateVimDir("vimfiles")
let s:pluginPath = CreateVimDir("vimfiles/plugins")
let s:localPlugins = fnameescape(expand(s:vimfilesDir . "/local_plugins.vim"))


"
let g:proseFileTypes = "'latex,context,plaintex,tex,
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
" gitgutter needs grep to not output escap sequences.
" let g:gitgutter_grep = ''
let g:gitgutter_grep = 'grep --color=never'
let g:gitgutter_override_sign_column_highlight = 0
let g:gitgutter_escape_grep = 1
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


" Separate buffer lists for differetn windows
" Plug 'https://github.com/zefei/vim-wintabs'
Plug 'https://github.com/tomtom/tcomment_vim'
let g:tcomment_opleader1='<leader>c'
Plug 'https://github.com/scrooloose/nerdtree.git'
Plug 'https://github.com/jacquesbh/vim-showmarks.git'
Plug 'https://github.com/vim-syntastic/syntastic.git'
" Adds a bunch of unix-mapped filesystem ops from vim
Plug 'https://github.com/tpope/vim-eunuch'
Plug 'https://github.com/simnalamburt/vim-mundo'
Plug 'https://github.com/vim-airline/vim-airline'
Plug 'https://github.com/vim-airline/vim-airline-themes'
" exec "Plug \'https://github.com/vim-airline/vim-airline-themes\', {\'rtp\' : \'autoload/airline/themes/". colorSch . ".vim\'}"
Plug 'https://github.com/ctrlpvim/ctrlp.vim'
let g:ctrlp_cmd = 'CtrlPMixed'
" For switching between header and alt files
Plug 'vim-scripts/a.vim'
" Plug 'https://github.com/vim-latex/vim-latex'
" let g:Tex_DefaultTargetFormat="pdf"
Plug 'https://github.com/lervag/vimtex'
" Run shell commands async (uses python)
Plug 'https://github.com/joonty/vim-do'
Plug 'https://github.com/python-mode/python-mode', { 'branch': 'develop' }
Plug 'https://github.com/thinca/vim-quickrun'
Plug 'https://github.com/vim-scripts/SingleCompile'
" Make is run async (view quickfix with :COpen)
Plug 'https://github.com/tpope/vim-dispatch'
" Way better search and replace, also case coersion
Plug 'https://github.com/tpope/vim-abolish'
" Improves incremental search to match everythign that it should.
Plug 'https://github.com/haya14busa/incsearch.vim'
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
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
" Let's give it a go then.
Plug 'https://github.com/easymotion/vim-easymotion'
Plug 'ryanoasis/vim-devicons'
" May cause lag on scrolling.
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'


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
"  Airoline handles status stuff (or should)
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
" let g:syntastic_stl_format = "[%E{Err: #%e L%fe}%B{, }%W{Warn: #%w L%fw}]"

let g:syntastic_enable_signs=1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_loc_list_height = 5
"ausot open errors wen present, closn when donet.
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" let g:syntastic_python_checkers = ['pylint']

let g:syntastic_error_symbol = 'X'
let g:syntastic_warning_symbol = "!"
augroup mySyntastic
    autocmd!
    au FileType tex let b:syntastic_mode = "passive"
augroup END
" TODO make this window-specific.
" let g:syntastic_shell = "/bin/sh"


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


" ----- majutsushi/tagbar settings -----
" Open/close tagbar with \b
nmap <silent> <leader>b :TagbarToggle<CR>
" Uncomment to open tagbar automatically whenever possible
"autocmd BufEnter * nested :call tagbar#autoopen(0)


" ----- Airline -----

" let g:airline_symbols_ascii=1
" Check if either of these have been specifically disabled or enabled.
if !exists('g:airline_powerline_fonts') || !exists('g:webdevicons_enable')
    " Automatically check for powerline compatible font installed locally
    " (unix) or to system (windows)
    " If we are (probably) using a powerline compatible font, set it so.
    " If a nerd font is found, assume powerline-compat, as well as devicons.
    if has("unix")
        let s:uname = system("uname")
        if s:uname == "Darwin\n"
            " OSX
            exec "let s:fontdir = expand('" . $HOME . "/Library/Fonts')"
        else
            " Linux
            exec "let s:fontdir = expand('" . $HOME . "/.fonts')"
        endif
    else
        " Windows
        exec "let s:fontdir = expand('" . $windir . "/Fonts')"
    endif

    let s:nerdFontNames = [
                \ "Sauce Code Pro Nerd Font Complete Mono Windows Compatible.ttf",
                \ "Sauce Code Pro Nerd Font Complete Windows Compatible.ttf",
                \ "Sauce Code Pro Nerd Font Complete Mono.ttf",
                \ "Sauce Code Pro Nerd Font Complete.ttf" ]
    let s:nerdFontIsInstalled = []
    let s:PLFontNames = [
                \ "SourceCodePro-Regular.ttf"]
    let s:PLFontIsInstalled = []

    let s:nerdFontExists = 0
    let s:PLFontExists = 0
    let i = 0
    while i < len(s:nerdFontNames)
        exec "call add(s:nerdFontIsInstalled, 
                    \ filereadable( expand('" . s:fontdir . "/" . s:nerdFontNames[i] . "')))"
        exec "let s:nerdFontExists = " . s:nerdFontExists . " || " . s:nerdFontIsInstalled[i]
        exec "let s:PLFontExists = " . s:nerdFontExists . " || " . s:nerdFontIsInstalled[i]
        let i += 1
    endwhile

    let i = 0
    if !s:nerdFontExists
        while i < len(s:PLFontNames)
            exec "call add(s:PLFontIsInstalled, 
                        \ filereadable( expand('" . s:fontdir . "/" . s:PLFontNames[i] . "')))"
            exec "let s:PLFontExists = " . s:PLFontExists . " || " . s:PLFontIsInstalled[i]
            let i += 1
        endwhile
    endif

    let s:guiUsesNerdFont = 
                \ &guifont =~ "Nerd" ||
                \ &guifont =~ "Sauce"

    let s:guiUsesPLFont = s:guiUsesNerdFont || 
                \ &guifont =~ "Powerline" ||
                \ &guifont =~ "Source\\ Code\\ Pro"

    if has("gui_running")
        let s:usePLFont = s:guiUsesPLFont
        let s:useNerdFont = s:guiUsesNerdFont
    else
        let s:usePLFont = s:PLFontExists
        let s:useNerdFont = s:nerdFontExists
    endif

    if !exists('g:airline_powerline_fonts')
        if s:usePLFont
            let g:airline_powerline_fonts = 1
        else
            let g:airline_powerline_fonts = 0
        endif
    endif

    if s:useNerdFont == 0
        if !exists('g:airline_powerline_fonts')
            " disable devicons and dependents.
            let g:NERDTreeDisableFileExtensionHighlight = 1
            let g:NERDTreeDisableExactMatchHighlight = 1
            let g:NERDTreeDisablePatternMatchHighlight = 1
            let g:webdevicons_enable = 0
        endif
    endif
endif

let g:airline#extensions#syntastic#stl_format_err="%E{Err: #%e L%fe}"
let g:airline#extensions#syntastic#stl_format_warn='%W{Warn: #%w L%fw}'
let g:airline_theme=colorSch
let g:airline#extensions#wordcount#enabled = 1
let g:airline_solarized_dark_inactive_border = 1
exec 'let g:airline_' . colorSch . '_bg="' . backgroundColor . '"'
" exec 'let g:airline_base16_' . colorSch . '= 0'
let base16colorspace = 256
exec 'let g:' . colorSch . '_base16 = 0'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 1
let g:airline#extensions#tabline#show_tabs = 0
" let g:airline#extensions#tabline#buffer_min_count = 0
" let g:airline#extensions#tabline#tab_min_count = 0
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#buffers_label = 'B'
let g:airline#extensions#whitespace#mixed_indent_algo = 1
" Alt sep gives b<c>b instead of b>c>b
let g:airline#extensions#tabline#alt_sep = 1
let g:airline#extensions#tabline# = 1
" let g:airline#extensions#tabline#show_tabs = 0
let g:airline_section_tabline = airline#section#create(['%{getcwd()}'])

if g:airline_powerline_fonts == 0
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
    let g:airline_section_z = airline#section#create([
                \ 'linenr', 'maxlinenr',' ', 'columnnr'])
else
    " Using predefined symbols
    " Skip gap between col symbol and number (custom section)
    call airline#parts#define_raw('linenr', g:airline_symbols.linenr . '%l')
    let g:airline_section_z = airline#section#create([
                \ 'linenr', 'maxlinenr',' ', '%c'])
endif

let g:airline#extensions#hunks#non_zero_only = 1
let g:airline#extensions#whitespace#checks = []
" Disable mode shown in cmdline
set noshowmode
"  reduce delay on insert leaave?
set ttimeoutlen=50

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


" ----------- TMUX --------------
" Prompt for a command to run
map <Leader>vp :VimuxPromptCommand<CR>
" Run last command executed by VimuxRunCommand
map <Leader>vl :VimuxRunLastCommand<CR>


" ----------- Misc --------------
call camelcasemotion#CreateMotionMappings('<leader>c')

let g:yankstack_yank_keys = ['c', 'C', 'd', 'D', 'x', 'X', 'y', 'Y']
call yankstack#setup()

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

nnoremap <leader>gc :Gwrite <bar> Gcommit<CR>
noremap <leader>gs :Gstatus<CR>

" Gif config
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)

let g:EasyMotion_startofline = 0 " keep cursor column when JK motion
"
" We need this for plugins like Syntastic(?) and vim-gitgutter which put symbols
" in the sign column. Don't know if it should go before plugs or after
" colorscheme.
" Allows hlcolumn bg to match coloursch
highlight clear SignColumn

