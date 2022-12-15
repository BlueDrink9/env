" Text object for the inner part of a stored procedure. Assumes only one per
" file, which I think is a spec requirement.
function! s:selectInnerStoredProcudure()
    silent! noautocmd norm! gg
    call search('\cprocedure\(\_s\|.\)*as\_s\+begin', 'eW')
    silent! noautocmd norm! j
    silent! noautocmd norm! V
    silent! noautocmd norm! G
    call search('\cEND$', 'bW')
    silent! noautocmd norm! k
endfunction
vnoremap isp :<C-U>call <sid>selectInnerStoredProcudure()<CR>
omap isp :normal Visp<CR>
