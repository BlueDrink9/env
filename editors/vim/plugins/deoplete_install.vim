if has("nvim")
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
  autocmd myPlugins User pluginSettingsToExec call
        \ deoplete#custom#option('yarp', v:true)
endif
Plug 'Shougo/denite.nvim'
" Includes listing known vim commands in denite. (also qf, ll, history, etc).
Plug 'https://github.com/neoclide/denite-extra'
" deoplete tab-complete - Supertab handles this.
let g:deoplete#enable_at_startup = 1
Plug 'https://github.com/deoplete-plugins/deoplete-lsp'
" Plug 'https://github.com/lionawurscht/deoplete-biblatex'
Plug 'deoplete-plugins/deoplete-tag'
" complete from syntax files
Plug 'Shougo/neco-syntax'
Plug 'Shougo/neoinclude.vim'
" Completion sources for vimscript
Plug 'Shougo/neco-vim'
Plug 'artur-shaik/vim-javacomplete2'
if executable("clang")
  Plug 'Shougo/deoplete-clangx', {'for': ['c', 'cpp'] }
endif
Plug 'deoplete-plugins/deoplete-dictionary'
" exec "Plug 'deoplete-plugins/deoplete-dictionary',
"             \ { 'for': " . g:proseFileTypes . " }"
" deoplete usually only completes from other buffers with the same
" filetype. This is a way of adding additional fts to complete from.
" Plug 'Shougo/context_filetype.vim'
