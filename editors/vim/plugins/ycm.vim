" v:version >= 740

let s:name = 'YouCompleteMe'
if has("macunix")
    " To allow a different binary to be compiled.
    let s:name=s:name . '-osx'
endif
function! s:BuildYCM(info)
    if (a:info.status == 'installed' || a:info.force)
        return
    endif
    let l:installOptions=[]
    let l:installSupports={
                \ "clangd" : "--clandg-completer",
                \ "go" : "--go-completer",
                \ "npm" : "--ts-completer",
                \ "rust" : "--rust-completer",
                \ "java" : "--java-completer",
                \ "mono" : "--cs-completer"
                \ }
    for prog in keys(l:installSupports)
        if executable(prog)
            call add(l:installOptions, l:installSupports[prog])
        endif
    endfor
    let l:installCmd="python3 ./install.py " . join(l:installOptions ," ")
    exec '!' . l:installCmd
    call plug#load("YouCompleteMe")
endfunction
let s:installOps={'do': function('s:BuildYCM'), 'on': [], 'dir': s:name}
call PlugOnInsertEnter('https://github.com/ycm-core/YouCompleteMe', 'YouCompleteMe', s:installOps)

let g:ycm_global_ycm_extra_conf = '~/.config/ycm/ycm_extra_conf.py'

" if executable('ctags-exuberant')
" ctags --version must be exuberant
if executable("ctags")
    let g:ycm_collect_identifiers_from_tags_files = 1
endif
let g:ycm_autoclose_preview_window_after_completion=1
" Let clangd fully control code completion
let g:ycm_clangd_uses_ycmd_caching = 0
" Use installed clangd, not YCM-bundled clangd which doesn't get updates.
let g:ycm_clangd_binary_path = exepath("clangd")
