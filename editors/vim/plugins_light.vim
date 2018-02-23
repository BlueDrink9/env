" vim: set ft=vim:

" Folder in which current script resides:
let s:path = expand('<sfile>:p:h')
let s:pluginPath = CreateVimDir("/vimfiles/plugins")

" for plugin in g:plugs
"     g:plugs.remove(plugin)
"     call remove(g:plugs, index)
" endfor

let g:plugs={}
let g:plugs_order=[]

call plug#begin(s:pluginPath)

Plug 'https://github.com/altercation/vim-colors-solarized.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'https://github.com/maxbrunsfeld/vim-yankstack.git'
Plug 'https://github.com/jlanzarotta/bufexplorer.git'
Plug 'https://github.com/tpope/vim-commentary'
Plug 'https://github.com/kien/rainbow_parentheses.vim'
Plug 'https://github.com/simnalamburt/vim-mundo'

Plug 'xolox/vim-misc'
Plug 'https://github.com/xolox/vim-session'
Plug 'https://github.com/lervag/vimtex'
Plug 'https://github.com/reedes/vim-pencil'

call plug#end()

