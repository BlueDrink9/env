" vim: set filetype=vim:
" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" Check out
" https://github.com/jalvesaq/Nvim-R/blob/03214225dd0467bb6724b38955f1c7ae5b439022/R/common_global.vim#L2792
" for more complex version of these funcs.
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

function! HasPython()
    return has('python') || has('python3') || has('pythonx')
endfunction

function! HasNvimPythonModule()
    if has('nvim')
        return has('python') || has('python3')
    endif
    try
        python3 import pynvim
    catch
        try
            python3 import neovim
        catch
            return v:false
        endtry
    endtry
    return v:true
endfunction

function! ExecutePlugMapping(mapping)
    execute "normal \<Plug>" . a:mapping
endfunction

function! IsCCompilerAvailable()
    " -- CCompilers = { vim.fn.getenv("CC"), "cc", "gcc", "clang", "cl", "zig" }
    let l:CCompilers = [ "cc", "gcc", "clang", "cl", "zig" ]
    for compiler in l:CCompilers
        if executable(compiler)
            return v:true
        endif
    endfor
    return v:false
endfunction

function! SetGFN(...)
    " If given an arg, use it as fontSize.
    if a:0 == 1
        " Override current gfn
        set gfn=
        let g:GUIFontSize = a:1
    else

        let g:GUIFontSize = g:defaultFontSize
    endif
    if &guifont == ""
        let l:formattedFonts = s:formatFontOptions(deepcopy(g:guiFonts), g:GUIFontSize)
        " Doesn't work with GTK, doesn't work unless set from GUIEnter
        " autocmd. That makes it pointless, since it's main use is to know
        " what the GUI font will be so smart character choices can be made
        " (powerline fonts etc).
        " call SetFirstValidGuifont(l:formattedFonts)
        let l:gfn=join(l:formattedFonts, ',')
        " Because we are using let, with a variable, spaces in font noames
        " don't need to be escaped.
        let &guifont=l:gfn

    endif
endfunction

function! s:formatFontOptions(list, size)
    let l:list=a:list
    let l:size=a:size
    if has("win32") || has("macunix") || has("nvim")
        let l:sizeSep=':h'
    else
        let l:sizeSep=' '
    endif
    for f in l:list
        let ind = index(l:list, f)
        let l:list[ind] = f . l:sizeSep . l:size
    endfor
    return l:list
endfunction

function! SetFirstValidGuifont(fonts)
    " preserve existing value
    let l:fontBackup = &guifont
    " let l:invalidFontError="/E596/"

    for font in a:fonts
        try
            let l:font=substitute(font, "\\ ", "\\\\ ", "g")
            exec "set guifont=" . l:font
            " echom "Applied GUI font: " . l:font
            return
        catch /E596/
            " echom v:exception
            continue
        endtry
    endfor
    " restore original value if a new valid one not found.
    exec "set guifont=" . substitute(l:fontBackup, "\\ ", "\\\\ ", "g")
endfunc

command! -nargs=+ -complete=command WindowOutput call myVimrcFunctions#WindowOutput(<q-args>)
nnoremap <F9> :w<enter> :myVimrcFunctions#WindowOutput !%:p<Enter>

inoremap <silent> <Tab> <C-R>=myVimrcFunctions#Tab_Or_Complete()<CR>
" Make imap if we want to remap s-tab normally.
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <S-Tab> <C-P>
" <CR> to confirm completion, use:
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<CR>"

nnoremap <silent> , :<C-U>call myVimrcFunctions#SingleCharInsert()<CR>

function! IsWSL()
    if !has('unix')
        return
    endif
    let s:version = system("cat /proc/version")
    if s:version=~"Microsoft"
        return 1
    else
        return 0
    endif
endfunction

" @ will play the macro over each line in visual range.
xnoremap @ :<C-u>call myVimrcFunctions#ExecuteMacroOverVisualRange()<CR>

command! -nargs=1 SearchAll call myVimrcFunctions#Vimgrepall(<f-args>)

command! -nargs=1 Mkdir call mkdir(<f-args>)

autocmd myVimrc BufNewFile *.rb 0put =\"#!/usr/bin/env ruby\<nl># -*- coding: None -*-\<nl>\"|$
autocmd myVimrc BufNewFile *.tex 0put =\"%&plain\<nl>\"|$
autocmd myVimrc BufNewFile *.\(cc\|hh\) 0put =\"//\<nl>// \".expand(\"<afile>:t\").\" -- \<nl>//\<nl>\"|2|start!

autocmd myVimrc BufWritePre * :call myVimrcFunctions#MkNonExDir(expand('<afile>'), +expand('<abuf>'))

" :W and :myVimrcFunctions#Save will escape a file name and write it
command! -bang -nargs=* W :call myVimrcFunctions#W(<q-bang>, <q-args>) 
command! -bang -nargs=* Save :call myVimrcFunctions#Save(<q-bang>, <q-args>) 

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

let g:GUIResizeValue=5
nnoremap <silent> <M-S-left> :call myVimrcFunctions#ResizeGUIHoriz(-g:GUIResizeValue)<cr>
nnoremap <silent> <M-S-right> :call myVimrcFunctions#ResizeGUIHoriz(g:GUIResizeValue)<cr>
nnoremap <silent> <M-S-up> :call myVimrcFunctions#ResizeGUIVert(g:GUIResizeValue)<cr>
nnoremap <silent> <M-S-down> :call myVimrcFunctions#ResizeGUIVert(-g:GUIResizeValue)<cr>

" Vim script to add user-defined words to spell file automatically
" A function to search for words after {marker_text} and add them to local
" spell file
function! AutoSpellGoodWords(marker_text)
    call cursor(1, 1)
    " let l:marker_text = '"%%  LocalWords:'
    let l:marker_text = a:marker_text

    let l:lines = []
    let l:goodwords_start = search(l:marker_text, 'cW')
    let l:line = getline(l:goodwords_start)
    let l:header = strpart(l:line, 0, strlen(l:marker_text))
    if l:header == l:marker_text
        call insert(l:lines, l:line)
    endif

    while l:goodwords_start > 0
        let l:goodwords_start = search(l:marker_text, 'W')
        if l:goodwords_start == 0
            break
        end
        let l:line = getline(l:goodwords_start)
        let l:header = strpart(l:line, 0, strlen(l:marker_text))
        if l:header == l:marker_text
            call insert(l:lines, l:line)
        endif
    endwhile

    let l:words = []
    for l:line in l:lines
        let l:line = strpart(l:line, strlen(l:marker_text) + 1)
        call extend(l:words, split(l:line))
    endfor
    for l:word in l:words
        silent execute ':spellgood! ' . l:word
    endfor
endfunction

" Comment with LocalWords at end of doc, similar to emacs.
autocmd myVimrc FileType tex call AutoSpellGoodWords('%  LocalWords:')

nnoremap yoy <cmd>call myVimrcFunctions#toggleSystemClipboard()<cr>
nnoremap yoa <cmd>call myVimrcFunctions#ToggleAutoWrite()<cr>


" Define command alias for at start of  command line mode only.
" https://github.com/vim-scripts/cmdalias.vim/blob/master/plugin/cmdalias.vim
command! -nargs=+ Alias :call CmdAlias(<f-args>)
function! CmdAlias(lhs, ...)
  if !exists('g:cmdaliasCmdPrefixes')
    let g:cmdaliasCmdPrefixes = 'verbose debug silent redir'
  endif
  let lhs = a:lhs
  " if lhs !~ '^\w\+$'
  "   echohl ErrorMsg | echo 'Only word characters are supported on <lhs>' | echohl NONE
  "   return
  " endif
  " if a:0 > 0
    let rhs = a:1
  " else
  "   echohl ErrorMsg | echo 'No <rhs> specified for alias' | echohl NONE
  "   return
  " endif
  " if has_key(s:aliases, rhs)
  "   echohl ErrorMsg | echo "Another alias can't be used as <rhs>" | echohl NONE
  "   return
  " endif
  if a:0 > 1
    let flags = join(a:000[1:], ' ').' '
  else
    let flags = ''
  endif
  exec 'cnoreabbr <expr> '.flags.a:lhs.
  \ " <SID>ExpandAlias('".lhs."', '".rhs."')"
endfunction


command! Profile call myVimrcFunctions#Profile()
command! ProfileStop profile stop

" Light, short backup for commenting plugins when they aren't available.
nnoremap <silent> <leader>cc :call myVimrcFunctions#ToggleComment()<cr>
vnoremap <silent> <leader>c :call myVimrcFunctions#ToggleComment()<cr>
nnoremap <silent> <leader>c :set opfunc=myVimrcFunctions#ToggleComment<CR>g@
