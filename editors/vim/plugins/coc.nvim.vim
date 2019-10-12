" vim: foldmethod=marker
" vim: foldmarker={[},{]}

Plug 'neoclide/coc.nvim'
UnPlug 'autozimu/LanguageClient-neovim'
UnPlug 'w0rp/ale'
let g:coc_config_home=g:plugindir

let s:coc_disabled_fts = "'
            \placeholder,
            \placeholder
            \'"
exec 'autocmd myPlugins filetype ' . s:coc_disabled_fts . 'let b:coc_enabled=0'

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
            "

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" {[} Mappings
" use <Plug>(coc-diagnostic-next) for all diagnostics.
call add(g:pluginSettingsToExec, 'nmap <silent> ]e <Plug>(coc-diagnostic-next-error)')
call add(g:pluginSettingsToExec, 'nmap <silent> [e <Plug>(coc-diagnostic-prev-error)')
" Unimpaired makes remapping tricky.
let g:nremap = {"]e": "<Plug>(coc-diagnostic-next-error)","[e": "<Plug>(coc-diagnostic-prev-error)" }

let g:coc_snippet_next = "<c-f>"
let g:coc_snippet_prev = "<c-b>"

" Use <leader>i for IDE.
" Open Coc action list.
nnoremap <leader>ia :CocAction<cr>
nnoremap <leader>in <Plug>(coc-rename)
nnoremap <leader>ir <Plug>(coc-references)
nnoremap <leader>if <Plug>(coc-refactor)
nnoremap <leader>id <Plug>(coc-definition)
nnoremap <leader>ii <Plug>(coc-implementation)
nnoremap gr <Plug>(coc-references)
nnoremap gd <Plug>(coc-definition)
nnoremap gy <Plug>(coc-type-definition)
nnoremap gi <Plug>(coc-implementation)
" gh - get hint on whatever's under the cursor
nnoremap <silent> K :call <SID>show_documentation()<CR>
nnoremap <silent> gh :call <SID>show_documentation()<CR>
nnoremap <silent> <leader>ih :call <SID>show_documentation()<CR>
" Use <leader>e for errors/linting/fixing.
nnoremap <leader>eca <Plug>(coc-codeaction)
vnoremap <leader>eca <Plug>(coc-codeaction-selected)
nnoremap <leader>ecl <Plug>(coc-codelens-action)
nnoremap <leader>ef <Plug>(coc-fix-current)
" List errors
nnoremap <silent> <leader>el  :<C-u>CocList locationlist<cr>

" Map <tab> to trigger completion and navigate to the next item:
function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction
inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<s-tab>"
" <CR> to confirm completion, use:
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<CR>"
" {]} Mappings

" hi CocErrorSign link WarningMsg
" hi CocWaringSign link WarningMsg
" hi CocInfoSign link WarningMsg
