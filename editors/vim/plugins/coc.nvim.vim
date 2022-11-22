" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" Use coc instead of ctags.
" This may mean tags aren't included, which would be a bug.
let g:vista_default_executive = "coc"

" Sets location of settigns json. Consider using coc#config() with a read in
" from a json instead, because otherwise you can't have a machine-local user
" config.
let g:coc_config_home=g:plugindir

let s:coc_disabled_fts = "'
      \placeholder,
      \placeholder
      \'"
exec 'autocmd myIDE filetype ' . s:coc_disabled_fts . 'let b:coc_enabled=0'

" Installed automatically by coc on startup!
" Lists gets qf, files, buffers, tags, etc.
" sh uses bash-language-server
" Consider re-adding coc-git for ability to stage chunks. Disable signifyr
let g:coc_global_extensions = [
      \ "coc-syntax",
      \ "coc-dictionary",
      \ "coc-snippets",
      \ "coc-omni",
      \ "coc-diagnostic",
      \ "coc-tag",
      \ "coc-gitignore",
      \ "coc-lists",
      \ "coc-sh",
      \ "coc-vimlsp",
      \ "coc-bibtex",
      \ "coc-vimtex",
      \ "coc-pyright",
      \ "coc-jedi",
      \ "coc-java",
      \ "coc-clangd",
      \ "coc-r-lsp"
      \ ]
" \ "coc-tabnine"
" latex lsp
" \ "coc-texlab",
"
if IsPluginUsed('neosnippet.vim')
  call add(g:coc_global_extensions, "coc-neosnippet")
elseif IsPluginUsed('ultisnips.vim')
  call add(g:coc_global_extensions, "coc-ultisnips")
endif

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" {[} Mappings
" Unimpaired makes remapping tricky.
let g:nremap = {"]e": "<Plug>(coc-diagnostic-next-error)","[e": "<Plug>(coc-diagnostic-prev-error)" }

" let g:coc_snippet_next = "<c-f>"
let g:coc_snippet_prev = "<c-b>"

" Use <leader>i for IDE.
" Open Coc action list.
call Nnoremap(g:IDE_mappings.allActions, ":CocAction<cr>")
call Nnoremap(g:IDE_mappings.allCommands, ":CocCommand<cr>")
call Nmap(g:IDE_mappings.rename, "<Plug>(coc-rename)")
call Nmap(g:IDE_mappings.references, "<Plug>(coc-references)")
call Nmap(g:IDE_mappings.references2, "<Plug>(coc-references)")
call Nmap(g:IDE_mappings.refactor, "<Plug>(coc-refactor)")
call Nmap(g:IDE_mappings.definition, "<Plug>(coc-definition)")
call Nmap(g:IDE_mappings.definition2, "<Plug>(coc-definition)")
call Nmap(g:IDE_mappings.typeDefinition, "<Plug>(coc-type-definition)")
call Nmap(g:IDE_mappings.implementation, "<Plug>(coc-implementation)")
call Nmap(g:IDE_mappings.implementation2, "<Plug>(coc-implementation)")
call Nmap(g:IDE_mappings.references, "<Plug>(coc-references)")
call Nmap(g:IDE_mappings.codeAction, "<Plug>(coc-codeaction)")
call Vmap(g:IDE_mappings.codeActionSelected, "<Plug>(coc-codeaction-selected)")
call Nmap(g:IDE_mappings.codelensAction, "<Plug>(coc-codelens-action)")
" Use <leader>e for errors/linting/fixing.
call Nmap(g:IDE_mappings.fix, "<Plug>(coc-fix-current)")
call Nmap(g:IDE_mappings.diagnosticNext, "<plug>(coc-diagnostic-next-error)")
call Nmap(g:IDE_mappings.diagnosticPrev, "<plug>(coc-diagnostic-prev-error)")
" List errors
exec 'nnoremap <silent> ' . g:IDE_mappings.listErrs . ' :<C-u>CocList locationlist<cr>'
exec 'nnoremap <silent> ' . g:IDE_mappings.documentation . ':call s:show_documentation()<CR>'
exec 'nnoremap <silent> ' . g:IDE_mappings.documentation2 . ':call s:show_documentation()<CR>'

let g:coc_snippet_next = g:IDE_mappings.snippetNext
let g:coc_snippet_prev = g:IDE_mappings.snippetPrev
" If we have tabbed to a snippet, can press space to expand it. Can't figure
" out how to include a literal space after...
inoremap <expr><space> coc#expandable() && coc#pum#visible() ? coc#pum#confirm() : ' '
call Imap(g:IDE_mappings.snippetExpand, "<Plug>(coc-snippets-expand-jump)")

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

function! s:cocTabOrComplete()
  return coc#pum#visible() ? coc#pum#next(1):
        \ <SID>check_back_space() ? "\<Tab>" :
        \ coc#refresh()
endfunction
inoremap <expr><Plug>MyCocRefresh <SID>cocTabOrComplete()
inoremap <silent><expr> <TAB> <SID>cocTabOrComplete()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
" let g:SuperTabDefaultCompletionType = "<Plug>MyCocRefresh"
" let g:completionCommand = <SID>cocTabOrComplete()

inoremap <cr> <cr>

" {]} Mappings

" au myIDE CursorHold,CursorHoldI * call CocAction('diagnosticInfo')

" hi CocErrorSign link WarningMsg
" hi CocWaringSign link WarningMsg
" hi CocInfoSign link WarningMsg
