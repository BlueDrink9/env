" vim: set filetype=vim:
" vim: foldmethod=marker
" vim: foldmarker={[},{]}


" Easier ways to use variables in remappings, instead of using exec.
function! Nnoremap(lhs, rhs)
  exec 'nnoremap ' . a:lhs . ' ' . a:rhs
endfunction
" command! -nargs=+ Nnoremap call Nnoremap(<f-args>)
function! Nmap(lhs, rhs)
  exec 'nmap ' . a:lhs . ' ' . a:rhs
endfunction
function! Vnoremap(lhs, rhs)
  exec 'vnoremap ' . a:lhs . ' ' . a:rhs
endfunction
function! Vmap(lhs, rhs)
  exec 'vmap ' . a:lhs . ' ' . a:rhs
endfunction
function! Inoremap(lhs, rhs)
  exec 'inoremap ' . a:lhs . ' ' . a:rhs
endfunction
function! Imap(lhs, rhs)
  exec 'imap ' . a:lhs . ' ' . a:rhs
  " imap <expr> a:lhs a:rhs
endfunction
" command! -nargs=+ Vnoremap call Vnoremap(<f-args>)

" Return text to call a local script outside the original.
" SID must come from the below s:SID function, recreated in the original
" script.
function! GetLocalFunctionCall(SID, func)
  return 'call ' . s:prefixSID(a:SID) . a:func
endfun
" Return the current script's <SID>. Taken from :h <SID>
function! s:SID()
  return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfun
function! s:prefixSID(SID)
  return '<SNR>' . a:SID . '_'
endfun

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
" TODO: Check pub for shift tab? Or incorporate shift tab?
function! Tab_Or_Complete()
    if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
        return "\<C-N>"
    else
        return "\<Tab>"
    endif
endfunction
inoremap <silent> <Tab> <C-R>=Tab_Or_Complete()<CR>
inoremap <S-Tab> <C-P>
" <CR> to confirm completion, use:
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<CR>"

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
let s:hidden_all = 0
function! ToggleHiddenAll()
    if s:hidden_all  == 0
        let s:hidden_all = 1
        set noshowmode
        set noruler
        set laststatus=0
        set noshowcmd
    else
        let s:hidden_all = 0
        set showmode
        set ruler
        set laststatus=2
        set showcmd
    endif
endfunction

" Uses an expandable signcolumn (neovim feature) to push text into the centre
" of the view.
" Sadly doesn't work for signcolumn size > 10.
function! CenterText()
  augroup CenterTextFunction
    au!
  augroup end
  let l:desiredTextWidth = 80
  if exists("s:storedSetting")
    let &signcolumn=s:storedSetting
    unlet s:storedSetting
    return
  endif
  let s:storedSetting=&signcolumn
  let l:excessWidth = winwidth(0) - l:desiredTextWidth
  if l:excessWidth < 1
    return
  endif
  " Use &columns for whole vim process, winwidth(0) for vim window
  let l:marginSize = (l:excessWidth) / 2
  let &signcolumn="yes:" . l:marginSize
endfunction

function! ResizeGUIVert(value)
    let &lines+=a:value
endfunction
function! ResizeGUIHoriz(value)
    let &columns+=a:value
endfunction
let g:GUIResizeValue=5
nnoremap <M-S-left> :call ResizeGUIHoriz(-g:GUIResizeValue)<cr>
nnoremap <M-S-right> :call ResizeGUIHoriz(g:GUIResizeValue)<cr>
nnoremap <M-S-up> :call ResizeGUIVert(-g:GUIResizeValue)<cr>
nnoremap <M-S-down> :call ResizeGUIVert(g:GUIResizeValue)<cr>
