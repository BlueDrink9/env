" vim: set filetype=vim:
" vim: foldmethod=marker
" vim: foldmarker={[},{]}

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

" function! HasNvimPythonModule()
"     if has('nvim')
"         return has('python') || has('python3')
"     endif
"     try
"         python3 import pynvim
"     catch
"         try
"             python3 import neovim
"         catch
"             return v:false
"         endtry
"     endtry
"     return v:true
" endfunction

function! IsCCompilerAvailable()
    " -- CCompilers = { vim.fn.getenv("CC"), "cc", "gcc", "clang", "cl", "zig" }
    let l:CCompilers = [ "cc", "gcc", "clang", "cl", "zig" ]
    for compiler in l:CCompilers
        if Executable(compiler)
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

function! IsWSL()
    if !has('unix')
        return
    endif
    return system("cat /proc/version") =~ "Microsoft"
endfunction

" " Shebangs
" autocmd myVimrc BufNewFile *.rb 0put =\"#!/usr/bin/env ruby\<nl># -*- coding: None -*-\<nl>\"|$
" autocmd myVimrc BufNewFile *.tex 0put =\"%&plain\<nl>\"|$
" autocmd myVimrc BufNewFile *.\(cc\|hh\) 0put =\"//\<nl>// \".expand(\"<afile>:t\").\" -- \<nl>//\<nl>\"|2|start!

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

command! CenterText call myVimrcFunctions#CenterText()

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


" Define command alias for at start of  command line mode only.
" https://github.com/vim-scripts/cmdalias.vim/blob/master/plugin/cmdalias.vim
command! -nargs=+ Alias :call CmdAlias(<f-args>)
function! CmdAlias(lhs, ...)
  if !exists('g:cmdaliasCmdPrefixes')
    let g:cmdaliasCmdPrefixes = 'verbose debug silent redir'
  endif
  let lhs = a:lhs
    let rhs = a:1
  if a:0 > 1
    let flags = join(a:000[1:], ' ').' '
  else
    let flags = ''
  endif
  exec 'cnoreabbr <expr> '.flags.a:lhs.
              \ " <SID>ExpandAlias('".lhs."', '".rhs."')"
endfunction

function! s:ExpandAlias(lhs, rhs)
  if getcmdtype() == ":"
    " Determine if we are at the start of the command-line.
    " getcmdpos() is 1-based.
    let partCmd = strpart(getcmdline(), 0, getcmdpos())
    " For each of the possible alias prefixes, create a list entry
    " containing a regex for that prefix at the start of the line with an
    " optional bang, surrounded by a space.
    let prefixes = ['^'] + map( split(g:cmdaliasCmdPrefixes, ' '), '"^" . v:val . "!\\? "')
    for prefix in prefixes
      if partCmd =~ prefix . a:lhs . '$'
        return a:rhs
      endif
    endfor
  endif
  return a:lhs
endfunction


function! s:ReadTemplate()
    filetype detect
    let l:templateDir = PathExpand(g:confDir . '/templates')
    let l:templatePaths = split(globpath(l:templateDir, &filetype . '.*'), '\n')
    for templatePath in l:templatePaths
      exec 'silent! 0read ' . templatePath
    endfor
endfunction
autocmd myVimrc BufNewFile * call s:ReadTemplate()
