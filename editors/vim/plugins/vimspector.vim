" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" See readme. Similar to visual studio, but F-keys only, no shifts or
" controls.
let g:vimspector_enable_mappings = 'HUMAN'
call Nmap(g:IDE_mappings.debugFile, "<Plug>VimspectorContinue")
call Nmap(g:IDE_mappings.debugContinue, "<Plug>VimspectorContinue")
call Nmap(g:IDE_mappings.debugRestart, "<Plug>VimspectorRestart")
call Nmap(g:IDE_mappings.debugStepInto, "<Plug>VimspectorStepOver")
call Nmap(g:IDE_mappings.debugStepOver, "<Plug>VimspectorStepInto")
call Nmap(g:IDE_mappings.debugStepOut, "<Plug>VimspectorStepOut")
call Nmap(g:IDE_mappings.debugRunToHere, "<Plug>VimspectorRunToCursor")
call Nmap(g:IDE_mappings.setBreakpoint, "<Plug>VimspectorToggleBreakpoint")
call Nmap(g:IDE_mappings.setBreakpointConditional, "<Plug>VimspectorToggleConditionalBreakpoint")
call Nmap(g:IDE_mappings.addBreakpointFunctional, "<Plug>VimspectorAddFunctionBreakpoint")
call Nmap(g:IDE_mappings.debugShowOutput, "<Plug>VimspectorShowOutput")
call Nmap(g:IDE_mappings.debugReset, "<Plug>VimspectorReset")
call Nmap(g:IDE_mappings.debugHover, "<Plug>VimspectorBalloonEval")
call Vmap(g:IDE_mappings.debugHover, "<Plug>VimspectorBalloonEval")
call Nmap(g:IDE_mappings.debugFrameUp, "<Plug>VimspectorUpFrame")
call Nmap(g:IDE_mappings.debugFrameDown, "<Plug>VimspectorDownFrame")
function! s:vimspectorSettings()
    nnoremap <buffer> q :VimspectorReset<CR>
    nnoremap <buffer> ! :VimspectorEval<CR>
    nnoremap <buffer> m :VimspectorWatch<CR>
    " May not want these if movement is useful in these buffers.
    nnoremap <buffer> e :VimspectorEval<CR>
    nnoremap <buffer> w :VimspectorWatch<CR>
endfunction
