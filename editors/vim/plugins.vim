" vim: set ft=vim:
" This file will source from light, main and ide plugin files, depending on
" the value of 2 variables. These should be set from command line via the
" --cmd option, eg $(vim --cmd "let g:liteMode=1" [file])

augroup myPlugins
    au!
augroup end

" {[} Settings and dir creation
" Skip loading this file entirely.
if exists("g:noPlugins")
    let g:liteMode=0
    let g:ideMode=0
    finish
endif

if !exists("g:liteMode")
    let g:liteMode=0
endif
if !exists("g:ideMode")
    let g:ideMode=0
endif

let s:vimfilesDir = CreateVimDir("vimfiles")
let s:pluginPath = CreateVimDir("vimfiles/plugins")
if v:version >= 703
    let s:scriptpath = fnameescape(expand('<sfile>:p:h'))
    let s:localPlugins = fnameescape(expand(s:vimfilesDir . "/local_plugins.vim"))
else
    let s:scriptpath = expand('<sfile>:p:h')
    let s:lpluginpathS=s:vimfilesDir . "/local_plugins.vim"
    let s:localPlugins = expand(s:lpluginpathS)
endif

if has('win32') || has ('win64')
    let $VIMHOME = $HOME."/vimfiles"
else
    let $VIMHOME = $HOME."/.vim"
endif
let s:autoloadDir=expand($VIMHOME . "/autoload")
let s:vimplug_file=expand(s:autoloadDir . "/plug.vim")
if !filereadable(s:vimplug_file)
    if executable("curl")
        let s:downloader = "!curl -fLo "
    elseif executable("wget")
        let s:downloader = "!wget --no-check-certificate -O "
    else
        echoerr "You have to install curl or wget, or install vim-plug yourself!"
        echoerr "vim-plug not installed. No plugins will be loaded."
        finish
    endif
    " Continue installing...
    exec "silent !mkdir -p " . s:autoloadDir
    echom "Installing Vim-Plug..."
    echo ""
    exec s:downloader . s:vimplug_file . " https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    if !filereadable(s:vimplug_file)
        echoerr "vim-plug failed to install. No plugins will be loaded."
        finish
    endif
    "   let g:not_finish_vimplug = "yes"
    autocmd myPlugins VimEnter * PlugInstall
endif

"
let g:proseFileTypes = "'latex,context,plaintex,tex,
            \markdown,mkd,
            \text,textile,
            \git,gitsendemail,
            \mail'"

if !filereadable(s:localPlugins)
    new
    silent exec 'write ' . s:localPlugins
    bdelete
endif

" To remove a Plugged repo using UnPlug 'user/repo'
function! s:deregister(repo)
    let repo = substitute(a:repo, '[\/]\+$', '', '')
    let name = fnamemodify(repo, ':t:s?\.git$??')
    call remove(g:plugs, name)
    call remove(g:plugs_order, index(g:plugs_order, name))
endfunction
command! -nargs=1 -bar UnPlug call s:deregister(<args>)

function! IsPluginUsed(name)
    if has_key(g:plugs, a:name)
        return 1
    else
        return 0
    endif
endfunction

cabbrev packi PlugInstall
cabbrev packu PlugUpdate <bar> PlugUpgrade

let g:pluginSettingsToExec = []
let g:customHLGroups = []

" Reset plugins entirely
" let g:plugs={}
" let g:plugs_order=[]
" {]}
call plug#begin(s:pluginPath)

" Get light plugin set first
exec 'source ' . s:scriptpath . "/colorschemes.vim"
exec 'source ' . s:scriptpath . "/light_plugins.vim"
if !g:liteMode
    exec 'source ' . s:scriptpath . "/main_plugins.vim"
    exec 'source ' . s:scriptpath . "/statusbar.vim"
    if g:ideMode
        exec 'source ' . s:scriptpath . "/ide_plugins.vim"
    endif
endif

" Unplugs and replacements go here
exec 'source ' . s:localPlugins

call plug#end()

for item in g:pluginSettingsToExec
    exec item
endfor

" HLGroups get cleared by colourschemes when changing. This resets them.
function! s:reHL()
    for item in g:customHLGroups
        exec "highlight! " . item
    endfor
endfunction
call s:reHL()
    autocmd myPlugins VimEnter,ColorScheme * call s:reHL()

if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
    if !g:hasGUI
        echom "Some plugins defined in config are uninstalled"
    endif
endif
