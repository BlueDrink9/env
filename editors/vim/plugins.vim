" vim: set ft=vim:

" Folder in which current script resides:
let s:path = expand('<sfile>:p:h')
let s:pluginPath = s:path . "/plugins"

call plug#begin(s:pluginPath)

" Maybe later, once I want them.
" s + 2 letters jumps to it (like 2 letter f or t, but vert)
" Plug 'https://github.com/justinmk/vim-sneak'
" Plug 'https://github.com/easymotion/vim-easymotion'
" Allows plugin maps to use '.' to repeat
" Plug 'https://github.com/tpope/vim-repeat'
" Way better search and replace
" Plug 'https://github.com/tpope/vim-abolish'
" Plug '

Plug 'https://github.com/altercation/vim-colors-solarized.git'
Plug 'https://github.com/tpope/vim-surround.git'
Plug 'https://github.com/maxbrunsfeld/vim-yankstack.git'
Plug 'https://github.com/scrooloose/nerdtree.git'
Plug 'https://github.com/jlanzarotta/bufexplorer.git'
Plug 'https://github.com/vim-scripts/ShowMarks.git'
Plug 'https://github.com/vim-syntastic/syntastic.git'
Plug 'https://github.com/Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'
Plug 'https://github.com/tpope/vim-eunuch'
Plug 'https://github.com/tpope/vim-commentary'

call plug#end()

" May be needed if terminal doesn't support.
let g:solarized_termcolors=256

" We need this for plugins like Syntastic and vim-gitgutter which put symbols
" in the sign column.
highlight clear SignColumn

" ----- scrooloose/syntastic settings -----
let g:syntastic_error_symbol = '✘'
let g:syntastic_warning_symbol = "▲"
augroup mySyntastic
  au!
  au FileType tex let b:syntastic_mode = "passive"
augroup END

" Change these if you feel the desire...
let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "✹",
    \ "Staged"    : "✚",
    \ "Untracked" : "✭",
    \ "Renamed"   : "➜",
    \ "Unmerged"  : "═",
    \ "Deleted"   : "✖",
    \ "Dirty"     : "✗",
    \ "Clean"     : "✔︎",
    \ 'Ignored'   : '☒',
    \ "Unknown"   : "?"
    \ }

