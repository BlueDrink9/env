" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" See readme. Similar to visual studio, but F-keys only, no shifts or
" controls.
let g:vimspector_enable_mappings = 'HUMAN'
call Nmap(g:IDE_mappings.debug_file, "<Plug>VimspectorContinue")
call Nmap(g:IDE_mappings.debug_continue, "<Plug>VimspectorContinue")
call Nmap(g:IDE_mappings.debug_restart, "<Plug>VimspectorRestart")
call Nmap(g:IDE_mappings.debug_step_into, "<Plug>VimspectorStepOver")
call Nmap(g:IDE_mappings.debug_step_over, "<Plug>VimspectorStepInto")
call Nmap(g:IDE_mappings.debug_step_out, "<Plug>VimspectorStepOut")
call Nmap(g:IDE_mappings.debug_run_to_here, "<Plug>VimspectorRunToCursor")
call Nmap(g:IDE_mappings.set_breakpoint, "<Plug>VimspectorToggleBreakpoint")
call Nmap(g:IDE_mappings.set_breakpoint_conditional, "<Plug>VimspectorToggleConditionalBreakpoint")
call Nmap(g:IDE_mappings.add_breakpoint_functional, "<Plug>VimspectorAddFunctionBreakpoint")
call Nmap(g:IDE_mappings.debug_show_output, "<Plug>VimspectorShowOutput")
call Nmap(g:IDE_mappings.debug_reset, "<Plug>VimspectorReset")
call Nmap(g:IDE_mappings.debug_hover, "<Plug>VimspectorBalloonEval")
call Vmap(g:IDE_mappings.debug_hover, "<Plug>VimspectorBalloonEval")
call Nmap(g:IDE_mappings.debug_frame_up, "<Plug>VimspectorUpFrame")
call Nmap(g:IDE_mappings.debug_frame_down, "<Plug>VimspectorDownFrame")
function! s:vimspectorSettings()
    nnoremap <buffer> q :VimspectorReset<CR>
    nnoremap <buffer> ! :VimspectorEval<CR>
    nnoremap <buffer> m :VimspectorWatch<CR>
    " May not want these if movement is useful in these buffers.
    nnoremap <buffer> e :VimspectorEval<CR>
    nnoremap <buffer> w :VimspectorWatch<CR>
endfunction
