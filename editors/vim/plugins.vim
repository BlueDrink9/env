" vim: set ft=vim:

" set laststatus=2

" Folder in which current script resides:
let s:scriptpath = fnameescape(expand('<sfile>:p:h'))
let s:vimfilesDir = CreateVimDir("vimfiles")
let s:pluginPath = CreateVimDir("vimfiles/plugins")
let s:localPlugins = fnameescape(expand(s:vimfilesDir . "/local_plugins.vim"))

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

" To remove a Plugged repo using UnPlug
function! s:deregister(repo)
    let repo = substitute(a:repo, '[\/]\+$', '', '')
    let name = fnamemodify(repo, ':t:s?\.git$??')
    call remove(g:plugs, name)
    call remove(g:plugs_order, index(g:plugs_order, name))
endfunction
command! -nargs=1 -bar UnPlug call s:deregister(<args>)

call plug#begin(s:pluginPath)

let g:pluginSettingsToExec = ['t',"echo test success"]
" Get light plugin set first
exec 'source ' . s:scriptpath . "/light_plugins.vim"

exec 'source ' . s:scriptpath . "/main_plugins.vim"

exec 'source ' . s:scriptpath . "/ide_plugins.vim"

" Unplugs and replacements go here
exec 'source ' . s:localPlugins

call plug#end()

for item in g:pluginSettingsToExec
    echom item
    exec item
endfor
