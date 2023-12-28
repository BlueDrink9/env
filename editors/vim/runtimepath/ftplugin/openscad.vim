setlocal foldmethod=marker
setlocal foldmarker={,}
setlocal commentstring=//%s

function s:openscad()
    if has('unix')
        silent !openscad "%" &
    else
        silent !start openscad "%"
    endif
endfunction
nnoremap <buffer><silent><expr> <leader>r <SID>openscad()
