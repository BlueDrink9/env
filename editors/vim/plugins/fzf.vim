" vim: foldmethod=marker
" vim: foldmarker={[},{]}

let g:mappings_dict = {
    \ g:IDE_mappings.FuzzyBuffers: '<cmd>Buffers<CR>',
    \ g:IDE_mappings.FuzzyOpenFile: '<cmd>FZF<CR>',
    \ g:IDE_mappings.FuzzySearchBuffer: '<cmd>BLines<CR>',
    \ g:IDE_mappings.FuzzySearchBuffers: '<cmd>Lines<CR>',
    \ g:IDE_mappings.FuzzyTags: '<cmd>Tags<CR>',
    \ g:IDE_mappings.FuzzyCommands: '<cmd>Commands<CR>',
    \ g:IDE_mappings.FuzzyOldFiles: '<cmd>History<CR>',
    \ g:IDE_mappings.FuzzySearchFiles: '<cmd>RG<CR>',
\ }

for [lhs, rhs] in items(g:mappings_dict)
    exec 'nnoremap ' . lhs . ' ' . rhs
endfor

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

autocmd myPlugins User MappingOverrides tnoremap <expr> <Esc> (&filetype == "fzf") ? "<Esc>" : "<c-\><c-n>"
