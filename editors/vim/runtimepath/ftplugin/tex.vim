" Do I really want this?
" autocmd BufNewFile *.tex 0put =\"%&plain\<nl>\"|$
inoremap <buffer> <c-b> \textbf{}<left>
" s = slope. c-i is tab, c-e is expand for ultisnips
inoremap <buffer> <c-s> \textit{}<left>
inoremap <buffer> <c-`> \texttt{}<left>
