" vim: set ft=vim:

inoremap kv <esc>
inoremap vk <esc>

" ctrl+S = save (otherwise unused)
noremap <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <C-O>:update<CR>

" Alias so that :W and :Q still work
:command! -bang W w<bang>
:command! -bang Q q<bang>

" :W! sudo saves the file 
" (useful for handling the permission-denied error)
" File needs to already exist.
command! -bang -nargs=* SudoSave w !sudo tee % > /dev/null
cmap W! SudoSave

" Don't have to hold shift for commands. Comes highly recommended.
nnoremap ; :
vnoremap ; :
nnoremap : ;
vnoremap : ;


" Easier way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" In insert or visual mode, use standard cut/copy/paste shortcuts.
" In normal mode, use ctrl+q
inoremap <C-v> <C-r>+
vnoremap <C-x> "+d
vnoremap <C-c> "+y
vnoremap <C-v> "+P
nnoremap <C-q> "+P

" Use CTRL-Q to do what CTRL-V used to do in insert
inoremap <C-Q> <C-V>

" CTRL-A is Select all in insert mode
inoremap <C-A> <C-O>gg<C-O>gH<C-O>G
