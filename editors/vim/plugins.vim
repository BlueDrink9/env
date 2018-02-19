" vim: set ft=vim:

" Folder in which current script resides:
let s:path = expand('<sfile>:p:h')
let s:pluginPath = s:path . "/plugins"

call plug#begin(s:pluginPath)

Plug 'https://github.com/altercation/vim-colors-solarized.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'https://github.com/maxbrunsfeld/vim-yankstack.git'
Plug 'https://github.com/scrooloose/nerdtree.git'
Plug 'https://github.com/jlanzarotta/bufexplorer.git'
Plug 'https://github.com/vim-scripts/ShowMarks.git'
Plug 'https://github.com/vim-syntastic/syntastic.git'

call plug#end()
