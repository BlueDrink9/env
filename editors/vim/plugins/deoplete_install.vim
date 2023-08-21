if has("nvim")
  Plugin 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plugin 'Shougo/deoplete.nvim'
  Plugin 'roxma/nvim-yarp'
  Plugin 'roxma/vim-hug-neovim-rpc'
  autocmd myPlugins User pluginSettingsToExec call
        \ deoplete#custom#option('yarp', v:true)
endif
Plugin 'Shougo/denite.nvim'
" Includes listing known vim commands in denite. (also qf, ll, history, etc).
Plugin 'https://github.com/neoclide/denite-extra'
" deoplete tab-complete - Supertab handles this.
let g:deoplete#enable_at_startup = 1
Plugin 'https://github.com/deoplete-plugins/deoplete-lsp'
" Plugin 'https://github.com/lionawurscht/deoplete-biblatex'
Plugin 'deoplete-plugins/deoplete-tag'
" complete from syntax files
Plugin 'Shougo/neco-syntax'
Plugin 'Shougo/neoinclude.vim'
" Completion sources for vimscript
Plugin 'Shougo/neco-vim'
Plugin 'artur-shaik/vim-javacomplete2'
if Executable("clang")
  Plugin 'Shougo/deoplete-clangx', {'for': ['c', 'cpp'] }
endif
Plugin 'deoplete-plugins/deoplete-dictionary'
" exec "Plugin 'deoplete-plugins/deoplete-dictionary',
"             \ { 'for': " . g:proseFileTypes . " }"
" deoplete usually only completes from other buffers with the same
" filetype. This is a way of adding additional fts to complete from.
" Plugin 'Shougo/context_filetype.vim'
