" vim: set ft=vim:

" Folder in which current script resides:
let s:scriptpath = fnameescape(expand('<sfile>:p:h'))
let s:pluginPath = CreateVimDir("/vimfiles/plugins")

" for plugin in g:plugs
"     g:plugs.remove(plugin)
"     call remove(g:plugs, index)
" endfor

let g:plugs={}
let g:plugs_order=[]

call plug#begin(s:pluginPath)

exec 'source ' . s:scriptpath . "/plugins_light.vim"
exec 'read ' . s:scriptpath . "/plugins_light.vim"

call plug#end()

