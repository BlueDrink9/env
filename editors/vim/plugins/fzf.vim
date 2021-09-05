" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" PlugInstall and PlugUpdate will clone fzf in ~/.fzf and run the install
" script
Plug 'junegunn/fzf', { 'dir': '~/.local/packages/fzf', 'do': './install --all' }
" Adds some vim-specific fzf commands.
Plug 'junegunn/fzf.vim'

" Fuzzy search list of buffers
nnoremap <silent> <leader><space> :Buffers<CR>
call Nnoremap(g:IDE_mappings.FuzzyBuffers, ":Buffers<CR>")
" Files
call Nnoremap(g:IDE_mappings.FuzzyOpenFile, ":FZF<CR>")
" Search lines in current buffer.
nnoremap <silent> <leader>/ :BLines<CR>
" Search lines in all buffers.
nnoremap <silent> <leader>f :Lines<CR>
nnoremap <silent> <leader>ft :Tags<CR>
" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1
command! MRUFZF :History

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

if has('nvim') || has('terminal')
  au myPlugins TermOpen * tnoremap <buffer> <Esc> <c-\><c-n>
  au myPlugins FileType fzf tunmap <buffer> <Esc>
endif
