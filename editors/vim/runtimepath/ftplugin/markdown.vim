vnoremap <Leader><Bar> :EasyAlign*<Bar><Enter>
" Shift+enter is soft new line in markdown.
inoremap <S-CR>   <CR>
nnoremap <S-CR> A  <esc>
if expand('%:p:h') =~# "joplin"
    " Fixes Joplin bug where vim's save method causes a file unlink.
    " https://github.com/laurent22/joplin/issues/710#issuecomment-473615087
    set backupcopy=yes
endif
