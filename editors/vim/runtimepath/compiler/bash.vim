if exists("current_compiler")
    finish
endif
setlocal makeprg=bash\ -n\ --\ %:S
setlocal errorformat=%f:\ line\ %l:\ %m
