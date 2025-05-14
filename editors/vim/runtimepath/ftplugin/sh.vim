" For folding to work properly with functions, apparently.
let g:sh_fold_enabled=5
let g:is_bash=1
set ts=2
set sw=2
augroup mySH
    au!
    autocmd BufNewFile *.sh 0put =\"#!/usr/bin/env bash\<nl>\"|$
augroup end
compiler bash
