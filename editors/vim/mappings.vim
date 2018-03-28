" vim: set ft=vim:

inoremap kv <esc>
inoremap vk <esc>

" Don't have to hold shift for commands. So nice
nnoremap ; :
vnoremap ; :
nnoremap : ;
vnoremap : ;

" let mapleader = "\<Space>"
" nnoremap <SPACE> <Nop>
map <SPACE> <leader>

" leader w opens new vert window, switches to it
nnoremap <leader>w <C-w>v<C-w>l
" Easier way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

"Faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

nnoremap ' `
nnoremap ` '

" Consistent with D, C
nnoremap Y y$

" :W! sudo saves the file
" (useful for handling the permission-denied error)
" File needs to already exist.
command! -bang -nargs=* SudoSave w !sudo tee % > /dev/null
cmap W! SudoSave

if bufwinnr(1)
    map + 10<C-W>>
    map - 10<C-W><
    map <kPlus> <C-W>+
    map <kMinus> <C-W>-
endif

" In insert or visual mode, use standard cut/copy/paste shortcuts.
" In normal mode, use ctrl+q
inoremap <C-v> <C-r>+
cnoremap <C-v> <C-r>+
vnoremap <C-x> "+d
vnoremap <C-c> "+y
vnoremap <C-v> "+P
nnoremap <C-q> "+P

" Use CTRL-Q to do what CTRL-V used to do in insert
inoremap <C-Q> <C-V>
" CTRL-A is Select all in insert mode
inoremap <C-a> <C-o>gg<C-o><S-V>G
" Spellcheck with completion list
nnoremap <leader>s ea<C-X><C-S>
" Remember cursor location and reformat file
nnoremap g= gg=G``
nnoremap gQ gggqG``
" Commented because vim-unimpaired gives similar mappings (=ol and h). yo
" enters PASTE mode.
" " Quick entry into paste
" nnoremap <leader>p :set paste!<CR>
" nnoremap <F1> :set hlsearch!<CR>
" nnoremap <leader>lc :set list!<CR>

" n and N always go the same direction regardless of whether / or ? was used.
nnoremap <expr> n  'Nn'[v:searchforward]
nnoremap <expr> N  'nN'[v:searchforward]
" Move through previous commands
nnoremap <expr> n  'Nn'[v:searchforward]
nnoremap <expr> N  'nN'[v:searchforward]
" Reset screen entirely (inc highlights)
nnoremap <leader>l :nohlsearch<cr>:diffupdate<cr>:syntax sync fromstart<cr><c-l>
" Quickly edit macros
nnoremap <leader>m  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>
" Don't lose selection on < or >
xnoremap <  <gv
xnoremap >  >gv
" Zoom a window into its own tab.
noremap <silent> <C-w>z :tab split<CR>
" Cd to current file
nnoremap <leader>cd :lcd %:p:h<CR>:pwd<CR>
" Rm spellcheck
map <RightMouse> z=
" Magic regex default
nnoremap / /\v
vnoremap / /\v
cnoremap %s/ %smagic/
cnoremap \>s/ \>smagic/
nnoremap :g/ :g/\v
nnoremap :g// :g//
" Insertmode deletes create an undopoint first
inoremap <c-u> <c-g>u<c-u>
inoremap <c-w> <c-g>u<c-w>
" Dot operator leaves cursor where it was
nmap . .`.
" Cycle through buffers
nnoremap <S-C-tab> :bp<CR>
nnoremap <C-tab> :bn<CR>
" Delete word under cursor, replace with pasted.
nnoremap <leader># "_diwP

" Use to overwrite mappings defined in plugins...
augroup override
au!

autocmd VimEnter * nnoremap Y y$

augroup END
