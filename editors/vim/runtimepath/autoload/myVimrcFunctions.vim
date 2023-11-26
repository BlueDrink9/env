function! myVimrcFunctions#Tab_Or_Complete() abort
    if !exists('g:completionCommand')
        let g:completionCommand="\<C-N>"
    endif
    " Use TAB to complete when typing words, else inserts TABs as usual.
    " Abort means func will abort if it detects an error.
    " If menu is already open the $(tab) cycles through suggested completions.
    if pumvisible()
        return "\<C-N>"
    elseif s:inWord()
        return g:completionCommand
    else
        return "\<Tab>"
    endif
endfunction
function! s:inWord()
    return col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
endfunction

function! myVimrcFunctions#RepeatChar(char, count)
    " Allow insertion of single character in normal mode.
    return repeat(a:char, a:count)
endfunction

function! myVimrcFunctions#SingleCharInsert()
    exec ":normal i".myVimrcFunctions#RepeatChar(nr2char(getchar()), v:count1)
endfunction

function! myVimrcFunctions#ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

function! myVimrcFunctions#Vimgrepall(pattern)
  " Search in all currently opened buffers. $(SearchAll [pattern])
  " Populates quicklist
  call setqflist([])
  exe 'bufdo vimgrepadd ' . a:pattern . ' %'
  cnext
endfunction

function! myVimrcFunctions#W(bang, filename) 
    :exe "w".a:bang." ". fnameescape(a:filename) 
endfunction

function! myVimrcFunctions#Save(bang, filename) 
    :exe "save".a:bang." ". fnameescape(a:filename) 
endfunction


function! myVimrcFunctions#toggleSystemClipboard()
  let l:clipboard="unnamed"
  if !exists("s:savedClipboardSetting")
    let s:savedClipboardSetting = &clipboard
  endif
  if &clipboard !=# l:clipboard
    let s:savedClipboardSetting = &clipboard
    let &clipboard=l:clipboard
  else
    let &clipboard = s:savedClipboardSetting
  endif
endfunction


function! myVimrcFunctions#Profile()
    let logfile = '$HOME/.logs/vim_profile.log'
    exec 'profile start ' . logfile
    profile func *
    profile! file *
    echom 'writing profile log to ' . logfile
    echom 'Run ProfileStop to end and write log'
endfunction

function! myVimrcFunctions#ToggleComment(...)
  if len(a:000) > 0
    " This was called as an opfunc. Apply to each line between the marks
    let l:start = getpos("'[")[1]
    let l:end = getpos("']")[1]
    for num in range(l:start, l:end)
      " jump to line
      exec 'norm! ' . num . 'gg'
      call myVimrcFunctions#ToggleComment()
    endfor
    return
  endif
  " Ensure regex special characters in commentstring (eg /*) are ignored by my
  " substitutes that include a .* to represent the line itself
  let l:commentstring = substitute(substitute(&commentstring,
        \ '\*', '\\*', 'g'),
        \ '\.', '\\.', 'g')
  " Capture group of .* to match entire line
  let l:search = substitute(l:commentstring, '%s', '\\(.*\\)', '')
  let l:line = getline('.')
  if match(l:line, l:search) >= 0
    " Uncomment - replace line with capture group for middle of search string
    " Matchlist[0] is the whole matched string, subsequent indices are capture
    " groups
    call setline('.',
          \ matchlist(l:line, l:search)[1])
  else
    " Comment - replace line with line inside comment string
    call setline('.', substitute(&commentstring, "%s", l:line, ''))
  endif
endfunction

function! myVimrcFunctions#MkNonExDir(file, buf)
    if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
        let dir=fnamemodify(a:file, ':h')
        if !isdirectory(dir)
            call mkdir(dir, 'p')
        endif
    endif
endfunction

function! myVimrcFunctions#WindowOutput(cmd)
    " Pipes the output of shell commands into a new window for viewing.
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

function! myVimrcFunctions#ResizeGUIHoriz(value)
    let &columns+=a:value
endfunction

function! myVimrcFunctions#ResizeGUIVert(value)
    let &lines+=a:value
endfunction

if exists('g:neovide')
    let g:neovide_scale_factor=1.0
    function! myVimrcFunctions#ChangeGFNSize(change)
        if !exists("g:GUIFontSize")
          let g:GUIFontSize = g:defaultFontSize
        endif
        let l:delta = (g:GUIFontSize + a:change) / (0.0 + g:GUIFontSize)
        let g:GUIFontSize += a:change
        let g:neovide_scale_factor = g:neovide_scale_factor * l:delta
        call SetGFN(g:GUIFontSize)
    endfunction
else
    function! myVimrcFunctions#ChangeGFNSize(change)
      if !exists("g:GUIFontSize")
        let g:GUIFontSize = g:defaultFontSize
      endif
      let g:GUIFontSize += a:change
      call SetGFN(g:GUIFontSize)
    endfunction
endif


" Uses an expandable signcolumn (neovim feature) to push text into the centre
" of the view.
" Sadly doesn't work for signcolumn size > 10.
function! myVimrcFunctions#CenterText()
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

