" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" Just install all available plugins for now...
Plug 'https://github.com/puremourning/vimspector', { 'do': ':!./install_gadget.py --all --disable-tcl' }
" See readme. Similar to visual studio, but F-keys only, no shifts or
" controls.
let g:vimspector_enable_mappings = 'HUMAN'
call Nmap(g:IDE_mappings.debug_file, "<F5>")
function! s:vimspectorSettings()
    nnoremap <buffer> q :VimspectorReset<CR>
    nnoremap <buffer> ! :VimspectorEval<CR>
    nnoremap <buffer> m :VimspectorWatch<CR>
    " May not want these if movement is useful in these buffers.
    nnoremap <buffer> e :VimspectorEval<CR>
    nnoremap <buffer> w :VimspectorWatch<CR>
endfunction
