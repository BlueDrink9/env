" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" See readme. Similar to visual studio, but F-keys only, no shifts or
" controls.
let g:vimspector_enable_mappings = 'HUMAN'
let g:mappings_dict = {
            \ g:IDE_mappings.addBreakpointFunctional: "<Plug>VimspectorAddFunctionBreakpoint",
            \ g:IDE_mappings.debugContinue: "<Plug>VimspectorContinue",
            \ g:IDE_mappings.debugFile: "<Plug>VimspectorContinue",
            \ g:IDE_mappings.debugFrameDown: "<Plug>VimspectorDownFrame",
            \ g:IDE_mappings.debugFrameUp: "<Plug>VimspectorUpFrame",
            \ g:IDE_mappings.debugHover: "<Plug>VimspectorBalloonEval",
            \ g:IDE_mappings.debugReset: "<Plug>VimspectorReset",
            \ g:IDE_mappings.debugRestart: "<Plug>VimspectorRestart",
            \ g:IDE_mappings.debugRunToHere: "<Plug>VimspectorRunToCursor",
            \ g:IDE_mappings.debugShowOutput: "<Plug>VimspectorShowOutput",
            \ g:IDE_mappings.debugStepInto: "<Plug>VimspectorStepOver",
            \ g:IDE_mappings.debugStepOut: "<Plug>VimspectorStepOut",
            \ g:IDE_mappings.debugStepOver: "<Plug>VimspectorStepInto",
            \ g:IDE_mappings.setBreakpoint: "<Plug>VimspectorToggleBreakpoint",
            \ g:IDE_mappings.setBreakpointConditional: "<Plug>VimspectorToggleConditionalBreakpoint",
            \ }

    for [lhs, rhs] in items(g:mappings_dict)
        exec 'nmap ' . lhs . ' ' . rhs
    endfor

function! s:vimspectorSettings()
    nnoremap <buffer> q :VimspectorReset<CR>
    nnoremap <buffer> ! :VimspectorEval<CR>
    nnoremap <buffer> m :VimspectorWatch<CR>
    " May not want these if movement is useful in these buffers.
    nnoremap <buffer> e :VimspectorEval<CR>
    nnoremap <buffer> w :VimspectorWatch<CR>
endfunction
