" Light, short backup for commenting plugins when they aren't available.
nnoremap <silent> <leader>cc :call myVimrcFunctions#ToggleComment()<cr>
vnoremap <silent> <leader>c :call myVimrcFunctions#ToggleComment()<cr>
nnoremap <silent> <leader>c :set opfunc=myVimrcFunctions#ToggleComment<CR>g@
