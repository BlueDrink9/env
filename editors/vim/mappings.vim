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

" x and X shouldn't overwrite the damn paste register!
nnoremap x "_x
nnoremap X "_X
" Toggle folds with backspace.
nnoremap <backspace> za
" Quicker access to system and unnamed registers
" (" is default register anyway, so never need "")
nnoremap "" "+
vnoremap "" "+
nnoremap """ "-
vnoremap """ "-
" black hole register delete
vnoremap <backspace> "_d
vnoremap <del> "_d
vnoremap x "_d
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
" Cd to current file
nnoremap <leader>cd <cmd>lcd %:p:h<CR>:pwd<CR>
" Autocomplete from tags
" inoremap <c-]> <c-x><c-]>
" Quickly re-run last macro with one key (also stop accidentally entering Ex).
nnoremap Q @@
" Used as a weird undo. Good candidate for remapping.
" nnoremap U <c-r>
" Include flags when redoing last :s command with &. Nvim default.
nnoremap & <Cmd>&&<CR>

" Good candidates for remapping...
" nnoremap <CR> ...
" nnoremap _ ...
" nnoremap - ...
" nnoremap + ...
" nnoremap R ...
" nnoremap U ...

" Select last paste/change.
nnoremap gp `[v`]

inoremap <C-BS> <C-w>
cnoremap <C-BS> <C-w>
" See s:remapCtrlBStoCW() for mappings in terminal. Not set here because
" they aren't 'basic'.

" Save/load entire buffer to/from clipboard
nnoremap <F5> <cmd>%y+<CR>
nnoremap <F9> <cmd>%d<CR>"+P
nnoremap <C-F5> <cmd>%y+<CR>
nnoremap <C-F9> <cmd>%d<CR>"+P
" {]} Basic mappings

" {[} Abbreviations and commands
" Abbreviations are used in insert and command modes unless specified.
abbrev <expr> [d] strftime("%Y-%m-%d")
abbrev <expr> [t] strftime("%H:%M")
cnoreabbrev hg helpgrep
cnoreabbrev vg vimgrep
cnoreabbrev H vert h
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
" :W! sudo saves the file
" (useful for handling the permission-denied error)
" File needs to already exist.
command! -bang -nargs=* SudoSave w !sudo tee % > /dev/null
cmap W! SudoSave
" Quickly edit macros
" command! -bang -nargs=* MacroEdit c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>
command! MRU browse oldfiles

" {]} Abbreviations

" {[} Window management
" Edit current buffer with a new tab
nnoremap <C-w>t <cmd>tab split<cr>
" Easier way to move between windows
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l

if $ISTERMUXSCREEN
  nnoremap <tab> <cmd>bnext<CR>
  " Don't remap arrow keys in termux, because they're too useful for the
  " touch screen.
else
  " Cycle through buffers
  nnoremap <silent> <Right> <cmd>bnext<CR>
  nnoremap <silent> <Left> <cmd>bprev<CR>
  nnoremap <silent> <Up> <cmd>tabnext<CR>
  nnoremap <silent> <Down> <cmd>tabprevious<CR>
endif

if has('nvim') || has('terminal')
  tnoremap <C-h> <c-\><c-n><c-w>h
  tnoremap <C-j> <c-\><c-n><c-w>j
  tnoremap <C-k> <c-\><c-n><c-w>k
  tnoremap <C-l> <c-\><c-n><c-w>l
endif

" Easy resize
nnoremap <S-Right> 5<C-W>>
nnoremap <S-Up> 3<C-W>+
nnoremap <C-Up> 3<C-W>-
nnoremap <C-Right> 5<C-W><
" Since you can't choose which side to expand/contract from, we'll reuse
" these for now.
nnoremap <S-Left> 5<C-W>>
nnoremap <S-Down> 3<C-W>+
nnoremap <C-Down> 3<C-W>-
nnoremap <C-Left> 5<C-W><
" Kill current buffer. Tab-complete bdel because may use Bdelete if available, not bdelete.
noremap <silent> <C-w>x <cmd>bdelete<tab><CR>

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
    " c-g u is to create an undo point first.
    inoremap <C-v> <c-g>u<C-r>+
    inoremap <C-S-v> <c-g>u<C-r>+
    cnoremap <C-v> <C-r>+
    cnoremap <C-S-v> <C-r>+
    vnoremap <C-X> "+d
    vnoremap <C-c> "+y
    " vnoremap <C-v> "+P  " Clobbers block visual
    " In normal mode, use ctrl+q
    nnoremap <C-q> "+P

    " Clipboardless workarounds are now set using vim-fakeclip, light plugin.
endif
" Use CTRL-Q to do what CTRL-V used to do in insert
inoremap <C-Q> <C-V>

" {]} Clipboard

" {[} pager
function! Pager()
  nnoremap <buffer> u <c-u>
  nnoremap <buffer> f <c-f>
  " nnoremap <buffer> b <c-b>
  nnoremap <buffer> d <c-d>
  nnoremap <buffer> <up> <c-y>
  nnoremap <buffer> <down> <c-e>
  " This doesn't have a local value
  " setlocal timeoutlen=20
endfunction
autocmd myVimrc bufwinenter * if ! &modifiable || &readonly | call Pager() | endif

" {]} pager

" {[} Misc
" Make entering escape in terminals non-impossible.
if has('nvim') || has('terminal')
  tnoremap <Esc> <C-\><C-n>
  tnoremap <C-v><Esc> <Esc>
  tnoremap <C-q><Esc> <Esc>
endif


if maparg(g:IDE_mappings.make, 'n') ==? ""
  call Nnoremap(g:IDE_mappings.make, "<cmd>w <bar> make<cr>")
endif

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

" move completion to next level.
cnoremap <C-e> <c-]><TAB>
" When autocompleting, start another file completion at this level.
inoremap <expr> <C-y> pumvisible() ? "<c-y><c-x><c-f>" : "C-y"
" Wildmenu: I use tab/s-tab for moving, so want to keep left/right.
cnoremap <Left> <Space><BS><Left>
cnoremap <Right> <Space><BS><Right>

" CTRL-A is Select all in insert mode, s is in visual
" Use f text obj plugin instead. Can manually do it if no plugins.
inoremap <a> <C-o>gg<C-o><S-V>G
" Spellcheck with completion list
" nnoremap <leader>s ea<C-X><C-S>
" Rm spellcheck
map <RightMouse> z=
" Magic regex default
nnoremap / /\v
vnoremap / /\v
cnoremap %s/ %smagic/
cnoremap \>s/ \>smagic/
" Because c-] doesn't work on colemak for some reason
nnoremap gt <c-]>
" Reset screen entirely (inc highlights)
command! Clear nohlsearch <bar> diffupdate <bar> syntax sync fromstart <bar> norm! <c-l>
" These apply only in vimdiff mode.
" nnoremap <expr> <C-J> &diff ? ']c' : '<C-W>j'
if &diff
    nnoremap <Leader>1 <cmd>diffget LOCAL<CR>
    nnoremap <Leader>2 <cmd>diffget BASE<CR>
    nnoremap <Leader>3 <cmd>diffget REMOTE<CR>
    nnoremap du <cmd>diffupdate<CR>
    cabbrev refresh diffupdate
endif

function! s:remapCtrlBStoCW()
    " Hopefully works in most GUIs, if not terminals.
    if g:hasGUI == 0
        if $TERM ==? "xterm-kitty" || $TERM_PROGRAM ==? "kitty"
            inoremap <C-H> <C-w>
            cnoremap <C-H> <C-w>
        elseif $TERM_PROGRAM ==? "mintty"
            inoremap <C-_> <C-w>
            cnoremap <C-_> <C-w>
        elseif has('nvim') && $TERM ==? "vtpcon"
            " Windows terminal console vim gets ^H for ctrl backspace.
            " Nvim sets term in this case, normal vim doesn't.
            inoremap <c-h> <C-w>
            cnoremap <c-h> <C-w>
        elseif has('nvim') && $TERM ==? "cygwin"
            " This seems to be the case with Windows Terminal and git commits.
            inoremap <BS> <C-w>
            cnoremap <BS> <C-w>
            inoremap <c-h> <bs>
            cnoremap <c-h> <bs>
        elseif has('win32') && $TERM ==? ""
            " Normal vim in windows console.
            " Wow. It sees the control key plus literal <c-?>.
            inoremap <c-> <C-w>
            cnoremap <c-> <C-w>
        else
            " Termux, maybe other xterms.
            inoremap <C-H> <C-w>
            cnoremap <C-H> <C-w>
        endif
    endif
endfunction
call s:remapCtrlBStoCW()

function! ChangeGFNSize(change)
  if !exists("g:GUIFontSize")
    let g:GUIFontSize = g:defaultFontSize
  endif
  let g:GUIFontSize += a:change
  call SetGFN(g:GUIFontSize)
endfunction
if g:hasGUI
  " These are what get sent in gvim for C-S-+ and C--
  nnoremap <expr> + ChangeGFNSize(1)
  " nnoremap <expr>  ChangeGFNSize(-1)
  " Because above doesn't work
  nnoremap <expr> - ChangeGFNSize(-1)
endif

" Replace visual selection with its evaluation result
vnoremap <silent> <c-r>= c<C-r>=<C-r>"<CR><ESC>

" {]} Misc
