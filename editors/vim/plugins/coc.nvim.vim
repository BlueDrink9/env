
Plug 'neoclide/coc.nvim'
UnPlug 'autozimu/LanguageClient-neovim'
UnPlug 'w0rp/ale'
let g:coc_config_home=g:plugindir

" Installed automatically by coc on startup!
" Lists gets qf, files, buffers, tags, etc.
" sh uses bash-language-server
let g:coc_global_extensions = [
            \ "coc-ultisnips",
            \ "coc-syntax",
            \ "coc-dictionary",
            \ "coc-omni",
            \ "coc-tag",
            \ "coc-gitignore",
            \ "coc-git",
            \ "coc-lists",
            \ "coc-sh",
            \ "coc-vimlsp",
            \ "coc-bibtex",
            \ "coc-vimtex",
            \ "coc-python",
            \ "coc-java",
            \ "coc-r-lsp"
            \ ]
            " \ "coc-tabnine"
            " latex lsp
            " \ "coc-texlab",

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction
inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
