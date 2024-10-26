setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal textwidth=79
setlocal expandtab
setlocal autoindent
setlocal fileformat=unix
setlocal encoding=utf-8
"python with virtualenv support
" py << EOF
" import os
" import sys
" if 'VIRTUAL_ENV' in os.environ:
"   project_base_dir = os.environ['VIRTUAL_ENV']
"   activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
"   execfile(activate_this, dict(__file__=activate_this))
" EOF
" let b:ale_linters = ['flake8']
" let b:ale_fixers = [
" \   'remove_trailing_lines',
" \   'isort',
" \   'ale#fixers#generic_python#BreakUpLongLines',
" \   'yapf',
" \]

" nnoremap <buffer> <silent> <LocalLeader>= :ALEFix<CR>

" " Quick run via <F5>
" " nnoremap <F5> :call <SID>compile_and_run()<CR>

" " function! s:compile_and_run()
" "     exec 'w'
" "     if &filetype == 'c'
" "         exec "AsyncRun! gcc % -o %<; time ./%<"
" "     elseif &filetype == 'cpp'
" "        exec "AsyncRun! g++ -std=c++11 % -o %<; time ./%<"
" "     elseif &filetype == 'java'
" "        exec "AsyncRun! javac %; time java %<"
" "     elseif &filetype == 'sh'
" "        exec "AsyncRun! time bash %"
" "     elseif &filetype == 'python'
" "        exec "AsyncRun! time python %"
" "     endif
" " endfunction
" " " asyncrun now has an option for opening quickfix automatically
" " let g:asyncrun_open = 15

" " Shebang
" autocmd <buffer> BufNewFile *.py 0put =\"#!/usr/bin/env python\<nl># -*- coding: iso-8859-15 -*-\<nl>\"|$
