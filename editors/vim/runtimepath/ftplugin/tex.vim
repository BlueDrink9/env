" Do I really want this?
" autocmd BufNewFile *.tex 0put =\"%&plain\<nl>\"|$
inoremap <buffer> <c-b> \textbf{}<left>
" s = slope. c-i is tab, c-e is expand for ultisnips
inoremap <buffer> <c-s> \textit{}<left>
" inoremap <buffer> <c-`> \texttt{}<left>
vnoremap <buffer> g<c-b> "zdi\textbf{}<esc>"zP
vnoremap <buffer> g<c-i> "zdi\textit{}<esc>"zP
vnoremap <buffer> g<c-m> "zdi\texttt{}<esc>"zP
nnoremap <buffer> g<c-b> "zdi\textbf{}<esc>"zP
nnoremap <buffer> g<c-i> "zdi\textit{}<esc>"zP
nnoremap <buffer> g<c-m> "zdi\texttt{}<esc>"zP
