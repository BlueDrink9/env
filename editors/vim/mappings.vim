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
vmap x "_d
vmap X "_d
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
" Emacs/cocoa BoL and EoL mappings. May remove later.
" imap <c-e> <Esc>A
" imap <c-a> <Esc>I
" command line
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-o>I <Home>
cnoremap <C-o>A <End>

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
" Quickly re-run last macro with one key (also stop accidentally entering Ex).
nnoremap Q @@
" Used as a weird undo. Good candidate for remapping.
nnoremap U <c-r>
" Good candidates...
" nnoremap <CR> ...
" nnoremap _ ...
" nnoremap - ...
" nnoremap + ...
" Select last paste/change.
nnoremap gp `[v`]
nnoremap K $
nnoremap gK K

inoremap <C-BS> <C-w>
cnoremap <C-BS> <C-w>
" See s:remapCtrlBStoCW() for mappings in terminal. Not set here because
" they aren't 'basic'.
" {]} Basic mappings

" {[} Abbreviations and commands
" Abbreviations are used in insert and command modes unless specified.
abbrev <expr> [d] strftime("%Y-%m-%d")
abbrev <expr> [t] strftime("%H:%M")
cnoreabbrev H helpgrep
" Opens an edit command with the dir of the currently edited file filled in.
" The C-R at the end is a hack. It swallows the space subsequently used to
" expand the abbreviation, meaning the cursor is left at the end of the
" path.
cabbrev le e <C-R>=expand("%:p:h") . "/" <CR><C-R>
cabbrev lr r <C-R>=expand("%:p:h") . "/" <CR><C-R>
" Load ide plugins/start ide mode.
cabbrev ide let g:ideMode=1 <bar> so $MYVIMRC
" Copy path of current buffer.
command! -bang -nargs=* PathCopy let @+ = expand("%:p")
" Word under cursor.
cabbrev <cw> <cword>
command! -bang -nargs=* Profile profile start $HOME/.logs/vim_profile <bar> profile func *
" :W! sudo saves the file
" (useful for handling the permission-denied error)
" File needs to already exist.
command! -bang -nargs=* SudoSave w !sudo tee % > /dev/null
cmap W! SudoSave
" Quickly edit macros
nnoremap <leader>m  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>
command! -bang -nargs=* Macros <c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>
cnoreabbrev H vert h
command! MRU browse oldfiles

" {]} Abbreviations

" {[} Window management
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
" Kill current buffer. Complete bdel because may use Bdelete, not bdelete.
noremap <silent> <C-w>x :bdel<tab><CR>
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
" On WSL, xclip exists but we don't want to use it.
if has("clipboard") && !IsWSL()
    " In insert or visual mode, use standard cut/copy/paste shortcuts.
    inoremap <C-v> <C-r>+
    cnoremap <C-v> <C-r>+
    vnoremap <C-X> "+d
    vnoremap <C-c> "+y
    " vnoremap <C-v> "+P  " Clobbers block visual
    " In normal mode, use ctrl+q
    nnoremap <C-q> "+P

    " Workarounds are now set using vim-fakeclip, light plugin.
endif

" {]} Clipboard

" {[} pager
function! Pager()
  nnoremap <buffer> u <c-u>
  nnoremap <buffer> f <c-f>
  nnoremap <buffer> b <c-b>
  nnoremap <buffer> d <c-d>
  nnoremap <buffer> <up> <c-y>
  nnoremap <buffer> <down> <c-e>
  " This doesn't have a local value
  " setlocal timeoutlen=20
endfunction
autocmd myVimrc bufwinenter * if ! &modifiable || &readonly | call Pager() | endif

" {]} pager

" {[} Misc

" Use CTRL-Q to do what CTRL-V used to do in insert
inoremap <C-Q> <C-V>

" Complete vim commands in cmd window.
" autocmd myVimrc CmdwinEnter * inoremap <buffer> <C-Space> <C-x><C-v>
function! s:cmdWinMappings()
  nnoremap <buffer> i i<c-c>
  nnoremap <buffer> I I<c-c>
  nnoremap <buffer> a a<c-c>
  nnoremap <buffer> A A<c-c>
  " Don't work with motions. Consider remapping with :map-operator.
  " nnoremap <buffer> C C<c-c>
  " nnoremap <buffer> c c<c-c>
endfunction
" Insert mode in cmdwin returns to actual cmd mode with current text.
autocmd myVimrc CmdwinEnter * call s:cmdWinMappings()

" c-y to move completion to next level.
cnoremap <C-y> <c-]><TAB>
" When autocompleting, start another file completion at this level.
inoremap <C-y> <c-y><c-x><c-f>

" Wildmenu: I use tab/s-tab for moving, so want to keep left/right.
cnoremap <Left> <Space><BS><Left>
cnoremap <Right> <Space><BS><Right>

" CTRL-A is Select all in insert mode, s is in visual
" Use f text obj plugin instead. Can manually do it if no plugins.
" inoremap <C-a> <C-o>gg<C-o><S-V>G
" vnoremap s <esc>gg<S-V>G
" Spellcheck with completion list
" nnoremap <leader>s ea<C-X><C-S>
" Rm spellcheck
map <RightMouse> z=
" Magic regex default
nnoremap / /\v
vnoremap / /\v
cnoremap %s/ %smagic/
cnoremap \>s/ \>smagic/
" fix typos I often make
nnoremap zQ ZQ
" nnoremap q; :q
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

function s:remapCtrlBStoCW()
    " Hopefully works in most GUIs, if not terminals.
    if g:hasGUI == 0
        if $TERM ==? "xterm-kitty" || $TERM_PROGRAM ==? "kitty"
            inoremap <C-H> <C-w>
            cnoremap <C-H> <C-w>
        elseif $TERM_PROGRAM ==? "mintty"
            inoremap <C-_> <C-w>
            cnoremap <C-_> <C-w>
        elseif has('nvim') && $TERM ==? "vtpcon"
            " Windows console vim gets ^H for normal backspace.
            " Nvim sets term in this case, normal vim doesn't.
            inoremap <BS> <C-w>
            cnoremap <BS> <C-w>
        elseif has('win32') && $TERM ==? ""
            " Normal vim in windows console.
            " Wow. It sees the control key plus literal <c-?>.
            inoremap <c-> <C-w>
            cnoremap <c-> <C-w>
        endif
    endif
endfunction
call s:remapCtrlBStoCW()

" {]} Misc
