" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" TODO: Do most of these as a function and autocmd on uienter or some
" other event, to prevent my mappings overriding these ones.
" {[} Mappings

inoremap kv :call VSCodeCall("vscode-neovim.escape") | echom "k v"
inoremap vk :call VSCodeCall("vscode-neovim.escape")

" {[} Window management
" leader w opens new vert window, switches to it
nnoremap <leader>w <C-w>v<C-w>l
" Edit current buffer with a new tab
nnoremap <C-w>t :tab sb<cr>
" No idea wtf this was meant to do...
" nnoremap <C-w><S-R> <W-w> <C-r>
" Easier way to move between windows
nnoremap <C-h> <Cmd>call VSCodeCall("workbench.action.focusLeftGroup")<CR>
nnoremap <C-l> <Cmd>call VSCodeCall("workbench.action.focusRightGroup")<CR>
nnoremap <C-k> <Cmd>call VSCodeCall("workbench.action.focusAboveGroup")<CR>
nnoremap <C-j> <Cmd>call VSCodeCall("workbench.action.focusBelowGroup")<CR>

" {[} Open windows to the left, right, up, down, like in tmux
nnoremap <C-w>h <Cmd>call VSCodeCall("workbench.action.splitEditorLeft")<CR>
nnoremap <C-w>l <Cmd>call VSCodeCall("workbench.action.splitEditorRight")<CR>
nnoremap <C-w>k <Cmd>call VSCodeCall("workbench.action.splitEditorUp")<CR>
nnoremap <C-w>j <Cmd>call VSCodeCall("workbench.action.splitEditorDown")<CR>
" {]} Open windows to the left, right, up, down.

" Cycle through buffers
nnoremap <silent> <Right> :call VSCodeCall("workbench.action.nextEditor")<CR>
nnoremap <silent> <Left> :call VSCodeCall("workbench.action.previousEditor")<CR>
nnoremap <silent> <Up> :tabnext<CR>
nnoremap <silent> <Down> :tabprevious<CR>

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

" {]} Window management

xmap <leader>c  <Plug>VSCodeCommentary
nmap <leader>c  <Plug>VSCodeCommentary
omap <leader>c  <Plug>VSCodeCommentary
nmap <leader>cc <Plug>VSCodeCommentaryLine

" Undo my \v mapping
nnoremap / /

nnoremap n n
nnoremap N N

" {]} Mappings

" Fix quickscope mappings (If used)
highlight QuickScopePrimary guifg='#afff5f' gui=underline ctermfg=155 cterm=underline
highlight QuickScopeSecondary guifg='#5fffff' gui=underline ctermfg=81 cterm=underline
