" Leader r to run current ahk script
nnoremap <buffer> <leader>r :!start /b autohotkey "%:p" <cr>
if g:ideMode == 1
  setl omnifunc=ahkcomplete#Complete
endif
