" vim: foldmethod=marker
" vim: foldmarker={[},{]}

Plug 'neoclide/coc.nvim', {'tag': '*', 'branch': 'release'}
let g:ale_enabled = 0
let g:LanguageClient_autoStart = 0
UnPlug 'davidhalter/jedi-vim'

" Use coc instead of ctags.
" This may mean tags aren't included, which would be a bug.
let g:vista_default_executive = "coc"

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
            \ "coc-ultisnips",
            \ "coc-syntax",
            \ "coc-dictionary",
            \ "coc-omni",
            \ "coc-tag",
            \ "coc-gitignore",
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
call Nnoremap(g:IDE_mappings["allActions"], ":CocAction<cr>")
call Nnoremap(g:IDE_mappings["rename"], "<Plug>(coc-rename)")
call Nnoremap(g:IDE_mappings["references"], "<Plug>(coc-references)")
call Nnoremap(g:IDE_mappings["references2"], "<Plug>(coc-references)")
call Nnoremap(g:IDE_mappings["refactor"], "<Plug>(coc-refactor)")
call Nnoremap(g:IDE_mappings["definition"], "<Plug>(coc-definition)")
call Nnoremap(g:IDE_mappings["definition2"], "<Plug>(coc-definition)")
call Nnoremap(g:IDE_mappings["type-definition"], "<Plug>(coc-type-definition)")
call Nnoremap(g:IDE_mappings["implementation"], "<Plug>(coc-implementation)")
call Nnoremap(g:IDE_mappings["implementation2"], "<Plug>(coc-implementation)")
call Nnoremap(g:IDE_mappings["references"], "<Plug>(coc-references)")
" Use <leader>e for errors/linting/fixing.
call Nnoremap(g:IDE_mappings["codeAction"], "<Plug>(coc-codeaction)")
call Vnoremap(g:IDE_mappings["codeActionSelected"], "<Plug>(coc-codeaction-selected)")
call Nnoremap(g:IDE_mappings["codelensAction"], "<Plug>(coc-codelens-action)")
call Nnoremap(g:IDE_mappings["fix"], "<Plug>(coc-fix-current)")
" List errors
exec 'nnoremap <silent> ' . g:IDE_mappings["listErrs"] . ' :<C-u>CocList locationlist<cr>'
exec 'nnoremap <silent> ' . g:IDE_mappings["documentation"] . ':call s:show_documentation()<CR>'
exec 'nnoremap <silent> ' . g:IDE_mappings["documentation2"] . ':call s:show_documentation()<CR>'
exec 'nnoremap <silent> ' . g:IDE_mappings["documentation3"] . ':call s:show_documentation()<CR>'

inoremap <expr><Plug>MyCocRefresh coc#refresh()
let g:SuperTabDefaultCompletionType = "<Plug>MyCocRefresh"
" {]} Mappings

" hi CocErrorSign link WarningMsg
" hi CocWaringSign link WarningMsg
" hi CocInfoSign link WarningMsg
