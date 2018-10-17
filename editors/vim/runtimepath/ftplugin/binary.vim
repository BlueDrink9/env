" edit binary files as hex. Define filetypes for the relevant extensions.
au BufReadPre  let &bin=1
au BufReadPost if &bin | %!xxd
au BufReadPost set ft=xxd | endif
au BufWritePre if &bin | %!xxd -r
au BufWritePre endif
au BufWritePost if &bin | %!xxd
au BufWritePost set nomod | endif
