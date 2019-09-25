" Do I really want this?
" autocmd BufNewFile *.tex 0put =\"%&plain\<nl>\"|$
inoremap <buffer> <c-b> \textbf{}<left>
inoremap <buffer> <c-e> \textit{}<left>
inoremap <buffer> <c-`> \texttt{}<left>
