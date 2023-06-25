" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" plugin firenvim in chrome and firefox.
" Open textframes in nvim, similar to wasavi.

" Configured as json, basically.
" disable by default. Manually activate with chrome binding.
" Use alt:all to always capture alt instead of sending a special key.
" Mainly for OSX. Use alphanum to ignore alt for alphanum.
" I don't use any alt mappings anyway, since terminals don't always
" support them.
if !exists("g:started_by_firenvim")
    finish
endif
let g:firenvim_config = {
    \ 'globalSettings': {
        \ 'alt': 'alphanum',
        \ 'cmdlineTimeout': 3000,
    \  },
    \ 'localSettings': {
        \ '.*': {
            \ 'priority': 0,
            \ 'selector': 'textarea:not([class="crayon-plain print-no"]), div[role="textbox"]',
            \ 'cmdline': 'firenvim',
            \ 'takeover': 'nonempty',
        \ },
    \ }
\ }
let s:fc = g:firenvim_config['localSettings']
let s:disabled_sites=[
            \ 'docs.google.com*',
            \ 'facebook.com*',
            \ 'deepl.com*',
            \ 'messenger.com*',
            \ 'twitter.com*',
            \ 'roll20.com*',
            \ 'habitica.com*',
            \ 'jira.*',
            \ ]
for site in s:disabled_sites
    let s:fc[site] = { 'priority': 1, 'selector': '', 'takeover': 'never' }
endfor

" The following options should only run for firenvim instances.
if !exists('g:started_by_firenvim')
    finish
endif

function! s:FirenvimSetPageOptions()
    " Use buffer mappings to ensure they override other mappings, even if
    " unlikely to change buffers in firenvim. Also future-proofs.
    let l:bufname=expand('%:t')
    " Use <LT>CR> to escape the CR and send it as a string, rather than
    " including it in the mapping. Needed for mapping to press_keys.
    if l:bufname =~? 'github.com'
        colorscheme github
        set ft=markdown
        let l:clickSubmitButtonJS = "'document.getElementById(\"partial-new-comment-form-actions\").getElementsByClassName(\"btn btn-primary\")[0].click();'"
        exec 'inoremap <buffer> <C-CR> <Esc>:w<CR>:call firenvim#eval_js(' . l:clickSubmitButtonJS . ')<CR>:q<CR>'
        exec 'nnoremap <buffer> <C-CR> :w<CR>:call firenvim#eval_js(' . l:clickSubmitButtonJS . ')<CR>:q<CR>'
    elseif l:bufname =~? 'cocalc.com' || l:bufname =~? 'kaggleusercontent.com'
        set ft=python
    elseif l:bufname =~? 'localhost' || l:bufname =~? '127.0.0.1'
        " Assume Jupyter notebook.
        set ft=python
    elseif l:bufname =~? 'reddit.com'
        set ft=markdown
    elseif l:bufname =~? 'stackexchange.com' || l:bufname =~? 'stackoverflow.com'
        set ft=markdown

    " Chat apps
    elseif l:bufname =~? 'slack.com' || l:bufname =~? 'gitter.im'
                \ || l:bufname =~? 'webchat.kde.org'
        set ft=markdown
        setlocal norelativenumber
        setlocal nonumber
        " For chat apps. Enter sents the message and deletes the buffer.
        " Shift enter is normal return.
        inoremap <buffer> <s-CR> <CR>
        " Insert mode start by default.
        normal! i
        if l:bufname =~? 'slack.com'
            " slack doesn't actually respond to press_keys (see firenvim readme).
            " Requires the send button to be enabled for this workspace.
            let l:clickSubmitButtonJS = "'document.getElementsByClassName(\"c-icon c-icon--paperplane-filled\")[0].click();'"
            exec 'inoremap <buffer> <CR> <Esc>:w<CR>:call firenvim#eval_js(' . l:clickSubmitButtonJS . ')<CR>ggdGa'
        else
            inoremap <buffer> <CR> <Esc>:w<CR>:call firenvim#press_keys("<LT>CR>")<CR>ggdGa
        endif
        inoremap <buffer> <s-CR> <CR>
    endif

endfunction


function! s:FirenvimSetGUIOptions()
    set showtabline=0
    set cmdheight=0
    if &lines < 2
        quitall!
    endif
    " Get rid of the annoying message at the bottom about the new file being
    " written, and then start insert mode.
    " Not working
    " autocmd myPlugins BufNewFile * silent redraw
    " autocmd myPlugins BufWrite * call feedkeys(";\<CR>")
    " autocmd myPlugins BufWritePost * call nvim_input(";<CR>")
    " This works
    " call feedkeys("i")
    colorscheme github
endfunction

function! SetFontSizeFirenvim(timer)
    call SetGFN()
endfunction

call timer_start(3000, function("SetFontSizeFirenvim"))

let s:debugMessages = []
function! s:firenvimSetup()
    autocmd myPlugins UIEnter * call s:FirenvimSetGUIOptions()
    " We are in firenvim
    " For debug messages during startup. After startup, use echom.
    if len(s:debugMessages) > 0
        autocmd myPlugins BufWinEnter * echom join(s:debugMessages, "\n")
    endif
    " call add(s:debugMessages, 'setup')
    let g:hasGUI=1
    " Tested to match github default size on arch bspwm brave.
    call SetGFN(9)
    set termguicolors
    call add(g:customHLGroups, 'EndOfBuffer guifg=guibg')
    " colorscheme PaperColor
    colorscheme github
    set background=light

    noremap <C-CR> <Esc>:w<CR>:call firenvim#press_keys("<LT>C-CR>")<CR>ggdG:q!
    nnoremap <C-z> :call firenvim#hide_frame()<cr>
    nnoremap <Esc><Esc><Esc> :call firenvim#focus_page()<CR>
    " Cmd + v on osx
    inoremap <D-v> <c-r>+
    cnoremap <D-v> <c-r>+
    vnoremap <D-c> "+y
    set colorcolumn=0

    au! myVimrc FocusLost,InsertLeave,BufLeave,CursorHold,CursorHoldI * ++nested silent! update

    autocmd myPlugins BufEnter *.txt call s:FirenvimSetPageOptions()
    " Auto-enter insertmode if the buffer is empty.
    autocmd myPlugins BufWinEnter * if line('$') == 1 && getline(1) == ''
                \ && bufname() != '' | startinsert | endif
   let g:strip_whitespace_on_save = 0
endfunction

" Fix odd bug that sometimes stops firenvim loading the text if setting a
" colourscheme from a plugin. Will be overridden in setup anyway.
let g:colorSch='default'
call s:firenvimSetup()
