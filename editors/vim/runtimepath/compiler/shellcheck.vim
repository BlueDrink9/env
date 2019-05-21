if exists("current_compiler")
    finish
endif
setlocal makeprg=shellcheck\ -s\ bash\ -f\ gcc\ --\ %:S
setlocal errorformat=%f:%l:%c:\ %m\ [SC%n]
