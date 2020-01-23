" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" PlugInstall and PlugUpdate will clone fzf in ~/.fzf and run the install
" script
Plug 'junegunn/fzf', { 'dir': '~/.local/packages/fzf', 'do': './install --all' }
" Adds some vim-specific commands.
Plug 'junegunn/fzf.vim'
" Both options are optional. You don't have to install fzf in ~/.fzf
" and you don't have to run the install script if you use fzf only in
" Vim.
" Files
nnoremap <leader><CR> :FZF<CR>
" Search lines in current buffer.
nnoremap <leader>/ :BLines<CR>
" Search lines in all buffers.
nnoremap <leader>f :Lines<CR>
nnoremap <leader>ft :Tags<CR>
" Open list of buffers
nnoremap <silent> <leader><space> :Buffers<CR>
" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1

" {[} Use proper fzf colours in gvim
if g:hasGUI
    let g:fzf_colors =
                \ { 'fg':      ['fg', 'Normal'],
                \ 'bg':      ['bg', 'Normal'],
                \ 'hl':      ['fg', 'Comment'],
                \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
                \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
                \ 'hl+':     ['fg', 'Statement'],
                \ 'info':    ['fg', 'PreProc'],
                \ 'border':  ['fg', 'Ignore'],
                \ 'prompt':  ['fg', 'Conditional'],
                \ 'pointer': ['fg', 'Exception'],
                \ 'marker':  ['fg', 'Keyword'],
                \ 'spinner': ['fg', 'Label'],
                \ 'header':  ['fg', 'Comment'] }
endif
" {]} Use proper fzf colours in gvim
