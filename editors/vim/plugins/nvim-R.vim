" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" Autostart when entering r files, if not already running.
" From plugin docs.
autocmd FileType r if string(g:SendCmdToR) == "function('SendCmdToR_fake')" | call StartR("R") | call <SID>SetnvimRShortcuts() | endif
autocmd FileType rmd if string(g:SendCmdToR) == "function('SendCmdToR_fake')" | call StartR("R") | call <SID>SetnvimRShortcuts() | endif
" And quit automatically
autocmd VimLeave * if exists("g:SendCmdToR") && string(g:SendCmdToR) != "function('SendCmdToR_fake')" | call RQuit("nosave") | endif
let r_syntax_folding = 1
let rrst_syn_hl_chunk = 1
let rmd_syn_hl_chunk = 1
let rout_follow_colorscheme = 1
let Rout_more_colors = 1
let R_rconsole_height = 5  " min height
let R_editing_mode = "vi"
" Make r tex recognised by tex plugins, at least for latex-box.
autocmd FileType rnoweb let b:main_tex_file = substitute(expand("%"), "\....$", ".tex", "")

command! RStart :call StartR("R") | call <SID>SetnvimRShortcuts()

" Need to create mock maps in order for nvim-R to create <plug> map on
" startup.
nnoremap <Plug>(Mock-nvim-R1) <Plug>RClearConsole
nnoremap <Plug>(Mock-nvim-R2) <Plug>RKnit

function! s:SetnvimRShortcuts()

    " {[} commands
    " Rstop is already defined by plugin.
    command! -buffer RStartCustom :call StartR("custom")
    command! -buffer RRunFile :call SendFileToR("echo")
    command! -buffer RRunToHere :execute 'normal Vggo<Esc>' | :call SendSelectionToR("echo", "down")
    command! -buffer RRunSelection :call SendSelectionToR("echo", "stay")
    command! -buffer RRunChunk :call SendChunkToR("echo", "down")
    command! -buffer RRunMotion :set opfunc=SendMotionToR<CR>g@
    command! -buffer RRunParagraph :call SendParagraphToR("echo", "down")
    command! -buffer RRunLine :call SendLineToR("down")
    command! -buffer RObjects :call RObjBrowser()
    command! -buffer RClearObjects :call RClearAll()
    command! -buffer RHelpMappings :help Nvim-R-use
    cabbrev <buffer> RR :exec "normal \<Plug>R"<left>
    "{]}

    " {[} mappings
    nnoremap <buffer> <localleader>h :RRunToHere<CR>
    inoremap <buffer> <C-f> <C-O>:RRunLine<CR>
    nnoremap <buffer> <C-p> :RRunLine<CR>
    exec 'nnoremap <buffer> ' . g:IDE_mappings.REPLCancel . ' :call RStop()'
    exec 'nnoremap <silent><buffer> ' . g:IDE_mappings.REPLSendLine . ' :RRunLine<CR>'
    exec 'noremap <silent><buffer> ' . g:IDE_mappings.REPLSend . ' :set opfunc=SendMotionToR<CR>g@'
    " exec 'nmap <buffer> ' . g:IDE_mappings.REPLSend . ' <Plug>RSendMotion'
    exec 'nmap <buffer> ' . g:IDE_mappings.REPLClear . ' <Plug>RClearConsole'
    exec 'nmap <buffer> ' . g:IDE_mappings.make . ' <Plug>RKnit'
    exec 'nmap <buffer> ' . g:IDE_mappings.documentation . ':call RAction("help")<CR>'
    exec 'vmap <buffer> ' . g:IDE_mappings.documentation . '<esc>:call RAction("help")<CR>'
    exec 'nmap <buffer> ' . g:IDE_mappings.documentation2 . ':call RAction("help")<CR>'
    exec 'vmap <buffer> ' . g:IDE_mappings.documentation2 . '<esc>:call RAction("help")<CR>'
    " {]} mappings

endfunction
