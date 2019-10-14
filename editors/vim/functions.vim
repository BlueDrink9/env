" vim: set filetype=vim:
" vim: foldmethod=marker
" vim: foldmarker={[},{]}


function! Nnoremap(lhs, rhs)
  exec 'nnoremap ' . a:lhs . ' ' . a:rhs
endfunction
" command! -nargs=+ Nnoremap call Nnoremap(<f-args>)

function! Vnoremap(lhs, rhs)
  exec 'vnoremap ' . a:lhs . ' ' . a:rhs
endfunction
" command! -nargs=+ Vnoremap call Vnoremap(<f-args>)


" Pipes the output of shell commands into a new window for viewing.
function! WindowOutput(cmd)
    redir => message
    silent execute a:cmd
    redir END
    if empty(message)
        echoerr "no output"
    else
        " use "new" instead of "tabnew" below if you prefer split windows instead of tabs
        vnew
        setlocal buftype=nofile bufhidden=wipe noswapfile nobuflisted nomodified
        silent put=message
    endif
endfunction
command! -nargs=+ -complete=command WindowOutput call WindowOutput(<q-args>)
nnoremap <F9> :w<enter> :WindowOutput !%:p<Enter>

"Use TAB to complete when typing words, else inserts TABs as usual.
function! Tab_Or_Complete()
    if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
        return "\<C-N>"
    else
        return "\<Tab>"
    endif
endfunction
inoremap <silent> <Tab> <C-R>=Tab_Or_Complete()<CR>
inoremap <S-Tab> <C-P>

" Allow insertion of single character in normal mode.
function! RepeatChar(char, count)
    return repeat(a:char, a:count)
endfunction
function! SingleCharInsert(iora)
    exec ":normal ".a:iora.RepeatChar(nr2char(getchar()), v:count1)
endfunction
nnoremap <silent> s :<C-U>call SingleCharInsert("i")<CR>
nnoremap <silent> S :<C-U>call SingleCharInsert("a")<CR>

function! IsWSL()
    let s:version = system("cat /proc/version")
    if s:version=~"Microsoft"
        return 1
    else
        return 0
    endif
endfunction

" @ will play the macro over each line in visual range.
xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>
function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

" Search in all currently opened buffers. $(SearchAll [pattern])
" Populates quicklist
function! ClearQuickfixList()
  call setqflist([])
endfunction
function! Vimgrepall(pattern)
  call ClearQuickfixList()
  exe 'bufdo vimgrepadd ' . a:pattern . ' %'
  cnext
endfunction
command! -nargs=1 SearchAll call Vimgrepall(<f-args>)

command! -nargs=1 Mkdir call mkdir(<f-args>)

autocmd myVimrc BufNewFile *.rb 0put =\"#!/usr/bin/env ruby\<nl># -*- coding: None -*-\<nl>\"|$
autocmd myVimrc BufNewFile *.tex 0put =\"%&plain\<nl>\"|$
autocmd myVimrc BufNewFile *.\(cc\|hh\) 0put =\"//\<nl>// \".expand(\"<afile>:t\").\" -- \<nl>//\<nl>\"|2|start!

function! s:MkNonExDir(file, buf)
    if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
        let dir=fnamemodify(a:file, ':h')
        if !isdirectory(dir)
            call mkdir(dir, 'p')
        endif
    endif
endfunction
autocmd myVimrc BufWritePre * :call s:MkNonExDir(expand('<afile>'), +expand('<abuf>'))

" :W and :Save will escape a file name and write it
command! -bang -nargs=* W :call W(<q-bang>, <q-args>) 
command! -bang -nargs=* Save :call Save(<q-bang>, <q-args>) 
function! W(bang, filename) 
    :exe "w".a:bang." ". fnameescape(a:filename) 
endfu
function! Save(bang, filename) 
    :exe "save".a:bang." ". fnameescape(a:filename) 
endfu
