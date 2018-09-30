" Leader r to run current ahk script
augroup autohotkeyft
    au!
    autocmd FileType autohotkey nnoremap <buffer> <leader>r :!start /b autohotkey "%:p" <cr>
    autocmd FileType autohotkey setl omnifunc=ahkcomplete#Complete
augroup end
