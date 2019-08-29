" vim: set ft=vim:
" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" Leaders are now set properly in settings, to ensure plugins (loaded after
" settings but before mappings) will use them properly.
" let mapleader = " "
" let maplocalleader = " b"

" {[} Basic mappings (usually worth copying to vim emulation programs).

inoremap kv <esc>
inoremap vk <esc>
cnoremap kv <c-f>
cnoremap vk <c-f>

" Don't have to hold shift for commands. So nice
nnoremap ; :
vnoremap ; :
nnoremap : ;
vnoremap : ;

" Make entering escape in terminals non-impossible.
if has('nvim') || has('terminal')
  tnoremap <Esc> <C-\><C-n>
  tnoremap <C-v><Esc> <Esc>
  tnoremap <C-q><Esc> <Esc>
endif

" x and X shouldn't overwrite the damn paste register!
nnoremap x "_x
nnoremap X "_X
" Toggle/create folds with backspace.
nnoremap <backspace> za
vnoremap <backspace> zf
" Quicker access to system and unnamed registers
" (" is default register anyway, so never need "")
nnoremap "" "+
vnoremap "" "+
nnoremap """ "-
vnoremap """ "-
" black hole register delete
vmap <backspace> "_d
vmap <del> "_d
"Faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>
" Swap mark jumps to make jump to column easier to press than jump to line.
nnoremap ' `
nnoremap ` '
" Consistent with D, C
nnoremap Y y$
" Remember cursor location and reformat file
nnoremap g= gg=G$()
nnoremap gQ gggqG$()
" Insertmode deletes create an undopoint first
inoremap <c-u> <c-g>u<c-u>
inoremap <c-w> <c-g>u<c-w>
" Dot operator leaves cursor where it was
nmap . .'.
" Don't lose selection on < or >
xnoremap <  <gv
xnoremap >  >gv
" Delete word under cursor, replace with pasted.
nnoremap <leader># "_diwP
" Autoexpand brackets when creating functions etc.
inoremap (<CR> (<CR>)<Esc>O
inoremap {<CR> {<CR>}<Esc>O
inoremap {; {<CR>};<Esc>O
inoremap {, {<CR>},<Esc>O
inoremap [<CR> [<CR>]<Esc>O
inoremap [; [<CR>];<Esc>O
inoremap [, [<CR>],<Esc>O
" Switch between the last two files.
nnoremap <Leader>a <C-^>

" n and N always go the same direction regardless of whether / or ? was used.
nnoremap <expr> n  'Nn'[v:searchforward]
nnoremap <expr> N  'nN'[v:searchforward]
" Move through previous commands
nnoremap <expr> n  'Nn'[v:searchforward]
nnoremap <expr> N  'nN'[v:searchforward]
" Cd to current file
nnoremap <leader>cd :lcd %:p:h<CR>:pwd<CR>
" Autocomplete from tags
inoremap <c-]> <c-x><c-]>

" {]} Basic mappings

" {[} Abbreviations and commands
" Abbreviations are used in insert and command modes unless specified.
abbrev <expr> [d] strftime("%Y-%m-%d")
abbrev <expr> [t] strftime("%H:%M")
cnoreabbrev H helpgrep
" Opens an edit command with the dir of the currently edited file filled in.
cabbrev le e <C-R>=expand("%:p:h") . "/" <CR>
" Load ide plugins/start ide mode.
cabbrev ide let g:ideMode=1 <bar> so $MYVIMRC
" Copy path of current buffer.
cabbrev pathCopy let @+ = expland("%")
" Word under cursor.
cabbrev <cw> <cword>

command! -bang -nargs=* Profile profile start resultfile <bar> profile func *
" :W! sudo saves the file
" (useful for handling the permission-denied error)
" File needs to already exist.
command! -bang -nargs=* SudoSave w !sudo tee % > /dev/null
cmap W! SudoSave
" Quickly edit macros
nnoremap <leader>m  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>
command! -bang -nargs=* Macros <c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>


" {]} Abbreviations

"{[} Window management
" leader w opens new vert window, switches to it
nnoremap <leader>w <C-w>v<C-w>l
nnoremap <C-w>t :tabnew<CR>
" No idea wtf this is meant to do...
nnoremap <C-w><S-R> <W-w> <C-r>
" Easier way to move between windows
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l

" Cycle through buffers
nnoremap <silent> <Right> :bnext<CR>
nnoremap <silent> <Left> :bprev<CR>
nnoremap <silent> <Up> :tabnext<CR>
nnoremap <silent> <Down> :tabprevious<CR>

if has('nvim') || has('terminal')
  tnoremap <C-h> <c-\><c-n><c-w>h
  tnoremap <C-j> <c-\><c-n><c-w>j
  tnoremap <C-k> <c-\><c-n><c-w>k
  tnoremap <C-l> <c-\><c-n><c-w>l
endif

" Easy resize
nnoremap <S-Right> 5<C-W>>
nnoremap <S-Left> 5<C-W><
nnoremap <S-Up> 3<C-W>+
nnoremap <S-Down> 3<C-W>-
" Zoom a window into its own tab.
noremap <silent> <C-w>z :tab split<CR>
" if has("gui")
"     " If window id of last window is 1, assume only one window present
"     if winnr($) == 1

" {[} Open windows to the left, right, up, down, like in tmux
function! s:SaveSplitSide()
    let s:splitSideH=&splitbelow
    let s:splitSideV=&splitright
endfunction
function! s:RestoreSplitSide()
    let &splitbelow=s:splitSideH
    let &splitright=s:splitSideV
endfunction
function! s:SplitLeft()
    call s:SaveSplitSide()
    set nosplitright
    vsplit
    call s:RestoreSplitSide()
endfunction
function! s:SplitRight()
    call s:SaveSplitSide()
    set splitright
    vsplit
    call s:RestoreSplitSide()
endfunction
function! s:SplitUp()
    call s:SaveSplitSide()
    set nosplitbelow
    split
    call s:RestoreSplitSide()
endfunction
function! s:SplitDown()
    call s:SaveSplitSide()
    set splitbelow
    split
    call s:RestoreSplitSide()
endfunction

nnoremap <C-w>h :call <SID>SplitLeft()<CR>
nnoremap <C-w>l :call <SID>SplitRight()<CR>
nnoremap <C-w>k :call <SID>SplitUp()<CR>
nnoremap <C-w>j :call <SID>SplitDown()<CR>
" {]} Open windows to the left, right, up, down.

" {]} Window management

" {[} Clipboard
if has("clipboard")
    " In insert or visual mode, use standard cut/copy/paste shortcuts.
    inoremap <C-v> <C-o>"+P
    cnoremap <C-v> <C-r>+
    vnoremap <C-X> "+d
    vnoremap <C-c> "+y
    " vnoremap <C-v> "+P  " Clobbers block visual
    " In normal mode, use ctrl+q
    nnoremap <C-q> "+P

    " {[} Workarounds
else
    " Replace with writing/reading from system commands in console vim.
    if exists('$CLIP_PROGRAM_COPY')
        let s:copy=$CLIP_PROGRAM_COPY
        let s:paste=$CLIP_PROGRAM_PASTE
    elseif has("win32")
        let s:paste = "paste.exe"
        let s:copy = "clip.exe"

    elseif has("unix")
        let s:uname = system("uname")
        if IsWSL()
            let s:paste = "paste.exe"
            let s:copy = "clip.exe"
            exec 'let s:pasteCMD = ":exec \"norm i\" . system(\"' . s:paste . '\")<CR><CR>
                \ <esc>kkkkkJJJJhi"'

        elseif s:uname =~ "Darwin"
            let s:paste = "pbpaste"
            let s:copy = "pbcopy"
        elseif $ISTERMUX " set in bash settings
            " Assume termux
            if executable('termux-get-clipboard')
                let s:paste = "termux-get-clipboard"
                let s:copy = "termux-set-clipboard"
            else
                echom "Termux-api not installed."
            endif
        else
            " Linux
            let s:paste = "xclip -o"
            let s:copy = "xclip"
        endif
    endif
    " exec 'let s:pasteCMD = ":let @j=system(\"' . s:paste . '\")<CR><CR>\"jp"'
    if !exists('s:pasteCMD')
        exec 'let s:pasteCMD = ":read !' . s:paste . '<CR><CR>"'
    endif
    " :exe 'norm i' . system("ls -l") inserts results at cursor, but with
    " paste still adds two newlines.
    " :exe 'norm i' . system("ls -l") inserts results at cursor, but with
    " <c-u> gets rid of range before calling.
    exec 'let s:copyCMD = ":w !' . s:copy . '<CR><CR>"'

    let g:clipboard = {
          \   'name': 'clipboard set in mappings',
          \   'copy': {
          \      '+': s:copy,
          \      '*': s:copy,
          \    },
          \   'paste': {
          \      '+': s:paste,
          \      '*': s:paste,
          \   },
          \   'cache_enabled': 1,
          \ }
    exec 'inoremap <C-v> <Esc>' . s:pasteCMD
    " exec 'cnoremap <C-v> <C-r>:read !' . s:paste . '<CR>'
    exec 'vnoremap <C-v> ' . s:pasteCMD
    exec 'nnoremap <C-q> ' . s:pasteCMD
    exec 'vnoremap <C-X> ' . s:copyCMD
    exec 'vnoremap <C-c> ' . s:copyCMD
endif
" {]} Workarounds
" {]} Clipboard

" {[} Misc

" Use CTRL-Q to do what CTRL-V used to do in insert
inoremap <C-Q> <C-V>

" CTRL-A is Select all in insert mode, s is in visual
inoremap <C-a> <C-o>gg<C-o><S-V>G
vnoremap s <esc>gg<S-V>G
" Spellcheck with completion list
nnoremap <leader>s ea<C-X><C-S>
" Rm spellcheck
map <RightMouse> z=
" Magic regex default
nnoremap / /\v
vnoremap / /\v
cnoremap %s/ %smagic/
cnoremap \>s/ \>smagic/
" fix typos I often make
nnoremap zQ ZQ
nnoremap q; :q
" Because c-] doesn't work on colemak for some reason
nnoremap <leader>t <c-]>
" Reset screen entirely (inc highlights)
nnoremap <leader>cl :nohlsearch<cr>:diffupdate<cr>:syntax sync fromstart<cr><c-l>
" These apply only in vimdiff mode.
" nnoremap <expr> <C-J> &diff ? ']c' : '<C-W>j'
if &diff
    nnoremap <Leader>1 :diffget LOCAL<CR>
    nnoremap <Leader>2 :diffget BASE<CR>
    nnoremap <Leader>3 :diffget REMOTE<CR>
    nnoremap du :diffupdate<CR>
    cabbrev refresh diffupdate
endif
" From the help for sections. Means { and } don't have to be at the start of
" the line.
" TODO, add more whynot. Do in filetype plugins, methinks.
" nnoremap [[ ?{<CR>w99[{
" nnoremap ][ /}<CR>b99]}
" nnoremap ]] j0[[%/{<CR>
" nnoremap [] k$][%?}<CR>
" Use [[ and ]] for fold markers!
" nnoremap [[ ?{[}<CR>w
" nnoremap ][ /{[}<CR>b
" nnoremap ]] j0/{]}<CR>
" nnoremap [] k${]}<CR>
" vnoremap [[ ?{[}<CR>w
" vnoremap ][ /{[}<CR>b
" vnoremap ]] j0/{]}<CR>
" vnoremap [] k${]}<CR>
" {]} Misc
