" Only enter insert mode if first line is empty
norm! gg
if getline('.') =~ '^\s*$'
    startinsert
endif
