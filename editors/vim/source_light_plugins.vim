" vim: set ft=vim:

" Folder in which current script resides:
let s:scriptpath = fnameescape(expand('<sfile>:p:h'))
let s:pluginPath = CreateVimDir("vimfiles/plugins")

" for plugin in g:plugs
"     g:plugs.remove(plugin)
"     call remove(g:plugs, index)
" endfor

let g:plugs={}
let g:plugs_order=[]

call plug#begin(s:pluginPath)

exec 'source ' . s:scriptpath . "/light_plugins.vim"
exec 'read ' . s:scriptpath . "/light_plugins.vim"

call plug#end()

" Settings to maximise speed/screen space
set cmdheight=1
set laststatus=0
set showmode
