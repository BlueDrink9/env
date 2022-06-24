" let g:ycm_global_ycm_extra_conf = '~/.config/ycm/ycm_extra_conf.py'

" if executable('ctags-exuberant')
" ctags --version must be exuberant
if executable("ctags")
    let g:ycm_collect_identifiers_from_tags_files = 1
endif
let g:ycm_confirm_extra_conf = 1
let g:ycm_autoclose_preview_window_after_completion=1

" if !IsPluginUsed("ultisnips")
"     let g:ycm_use_ultisnips_completer = 0
" endif

" Let clangd fully control code completion
let g:ycm_clangd_uses_ycmd_caching = 0
" Use installed clangd, not YCM-bundled clangd which doesn't get updates.
let g:ycm_clangd_binary_path = exepath("clangd")
let g:ycm_min_num_of_chars_for_completion = 3
let g:ycm_max_num_candidates = 30
let g:ycm_filetype_whitelist = {'*': 1}
let g:ycm_filetype_blacklist = {
            \ 'tagbar': 1,
            \ 'notes': 1,
            \ 'markdown': 1,
            \ 'netrw': 1,
            \ 'unite': 1,
            \ 'text': 1,
            \ 'vimwiki': 1,
            \ 'pandoc': 1,
            \ 'infolog': 1,
            \ 'leaderf': 1,
            \ 'mail': 1
            \}
let g:ycm_error_symbol = 'X'
let g:ycm_warning_symbol = '!'
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1

let g:ycm_key_invoke_completion = '<C-x><c-x>'
let g:ycm_key_detailed_diagnostics = g:IDE_mappings.codelensAction

function! s:SetShortcuts()
    call Nnoremap(g:IDE_mappings.definition, ":YcmCompleter GoTo<CR>")
    call Nnoremap(g:IDE_mappings.definition2, ":YcmCompleter GoTo<CR>")
    call Nnoremap(g:IDE_mappings.type_definition, ":YcmCompleter GoToType<CR>")
    call Nnoremap(g:IDE_mappings.rename, ":YcmCompleter RefactorRename<CR>")
    call Nnoremap(g:IDE_mappings.reformat, ":YcmCompleter Format<CR>")
    call Nnoremap(g:IDE_mappings.references, ":YcmCompleter GoToReferences<CR>")
    call Nnoremap(g:IDE_mappings.references2, ":YcmCompleter GoToReferences<CR>")
    call Nnoremap(g:IDE_mappings.fix, ":YcmCompleter FixIt<CR>")
    " call Nnoremap(g:IDE_mappings.complete, ":YcmCompleter textDocument_completion()<CR>")
    call Nnoremap(g:IDE_mappings.documentation, ":YcmCompleter GetDoc<CR>")
    call Nnoremap(g:IDE_mappings.documentation2, ":YcmCompleter GetDoc<CR>")
    call Nnoremap(g:IDE_mappings.documentation3, ":YcmCompleter GetDoc<CR>")
endfunction()
au myIDE InsertEnter call <SID>SetShortcuts()
