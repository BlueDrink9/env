" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" Installation {[}
" plugin firenvim in chrome and firefox.
" Open textframes in nvim, similar to wasavi.
if has('win32')
    let s:startup_prologue='"set LITE_SYSTEM=1"'
else
    let s:startup_prologue='"export LITE_SYSTEM=1"'
endif
let g:firenvim_install=":call firenvim#install(0, " . s:startup_prologue . ")"
" Only on tags/releases, because updates may require the extension to be
" updated.
Plug 'https://github.com/glacambre/firenvim', {'tag': '*', 'do': g:firenvim_install}
" Installation {]}

" Configured as json, basically.
" disable by default. Manually activate with chrome binding.
" Use alt:all to always capture alt instead of sending a special key.
" Mainly for OSX. Use alphanum to ignore alt for alphanum.
" I don't use any alt mappings anyway, since terminals don't always
" support them.
let g:firenvim_config = {
    \ 'globalSettings': {
        \ 'alt': 'alphanum',
    \  },
    \ 'localSettings': {
        \ '.*': {
            \ 'priority': 0,
            \ 'selector': 'textarea, div[role="textbox"]',
            \ 'cmdline': 'firenvim',
            \ 'takeover': 'nonempty',
        \ },
    \ }
\ }
let s:fc = g:firenvim_config['localSettings']
let s:disabled_sites=[
            \ 'docs.google.com*',
            \ 'facebook.com*',
            \ ]
for site in s:disabled_sites
    let s:fc[site] = { 'priority': 1, 'selector': '', 'takeover': 'never' }
endfor

" The following options should only run for firenvim instances.
if !exists('g:started_by_firenvim')
  finish
endif

function! s:isConnectedToFirenvim(channel)
    let l:ui = nvim_get_chan_info(a:channel)
    if !(has_key(l:ui, 'client') &&
                \ has_key(l:ui.client, 'name') &&
                \ l:ui.client.name ==# 'Firenvim')
        return v:false
    endif
    return v:true
endfunction

function! s:onFirenvimLoad()
  " call feedkeys("\<C-L>", 'n')
  " call s:firenvimSetup()
endfunction

function! s:firenvimSetup()
  " We are in firenvim
    let g:hasGUI=1
    call SetGFN(12)
    set termguicolors
    call add(g:customHLGroups, 'EndOfBuffer guifg=guibg')
    " colorscheme PaperColor
    colorscheme github
    set colorcolumn=0
    if &lines < 18
        let g:loaded_airline = 1
        silent! AirlineToggle
        " See neovim #1004
        " set cmdheight=0
        set cmdheight=1
        set laststatus=0
        set noshowmode
        set noruler
        set noshowcmd
        set shortmess=aWAFtI
        " Can't afford to hard wrap by mistake.
        set textwidth=200
    endif
    if &columns < 15
        set nonumber
        set norelativenumber
    endif

    " Get rid of the annoying message at the bottom about the new file being
    " written, and then start insert mode.
    " Not working
    " autocmd myPlugins BufNewFile * silent redraw
    " autocmd myPlugins BufWrite * call feedkeys(";\<CR>")
    " autocmd myPlugins BufWritePost * call nvim_input(";<CR>")
    " This works
    " call feedkeys("i")

    nnoremap <C-z> :call firenvim#hide_frame()<cr>
    nnoremap <Esc><Esc><Esc> :call firenvim#focus_page()<CR>
    au! myVimrc FocusLost,InsertLeave,BufLeave * ++nested call Autosave()
    autocmd myPlugins BufNewFile *.txt call s:FirenvimSetPageOptions()
endfunction

function! s:FirenvimSetPageOptions()
    let l:bufname=expand('%:t')
    if l:bufname =~? 'github.com'
        colorscheme github
        set ft=markdown
        " C-CR to send comment.
        l:githubSendComment="<Esc>:w<CR>:call firenvim#press_keys("<tab><tab><tab>CR>")<CR>ggdGa"
        exec 'inoremap <C-CR> ' . l:githubSendComment
        exec 'nnoremap <C-CR> ' . l:githubSendComment
    elseif l:bufname =~? 'cocalc.com' || l:bufname =~? 'kaggleusercontent.com'
        set ft=python
    elseif l:bufname =~? 'localhost'
        " Assume Jupyter notebook.
        set ft=python
    elseif l:bufname =~? 'reddit.com'
        set ft=markdown
    elseif l:bufname =~? 'stackexchange.com' || l:bufname =~? 'stackoverflow.com'
        set ft=markdown
    elseif l:bufname =~? 'slack.com' || l:bufname =~? 'gitter.com'
        " for chat apps. Enter sents the message and deletes the buffer.
        " Shift enter is normal return. Insert mode by default.
        normal! i
        inoremap <CR> <Esc>:w<CR>:call firenvim#press_keys("<LT>CR>")<CR>ggdGa
        inoremap <s-CR> <CR>
    endif
endfunction

autocmd myPlugins User pluginSettingsToExec call s:firenvimSetup()
autocmd myPlugins UIEnter * if s:isConnectedToFirenvim(deepcopy(v:event.chan)) | call s:onFirenvimLoad() | endif
