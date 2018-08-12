" vim: set ft=vim:
" This file will source from light, main and ide plugin files, depending on
" the value of 2 variables. These should be set from command line via the
" --cmd option, eg `vim --cmd "let g:liteMode=1" [file]`

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

" Reset plugins entirely
" let g:plugs={}
" let g:plugs_order=[]
" {]}
call plug#begin(s:pluginPath)

let g:pluginSettingsToExec = []
let g:customHLGroups = []

" Get light plugin set first
exec 'source ' . s:scriptpath . "/light_plugins.vim"
if !g:liteMode
    exec 'source ' . s:scriptpath . "/main_plugins.vim"
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
augroup highlights
    au!
    autocmd ColorScheme * call s:reHL()
augroup end
