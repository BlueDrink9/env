" For folding to work properly with functions, apparently.
let g:sh_fold_enabled=5
let g:is_bash=1
autocmd BufNewFile *.sh 0put =\"#!/usr/bin/env bash\<nl>\"|$
