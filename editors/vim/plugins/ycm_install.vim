let s:name = 'YouCompleteMe'
if has("macunix")
    " To allow a different binary to be compiled.
    let s:name=s:name . '-osx'
endif
function! BuildYCM()
    let l:installOptions=[]
    let l:installSupports={
                \ "clangd" : "--clangd-completer",
                \ "go" : "--go-completer",
                \ "npm" : "--ts-completer",
                \ "rust" : "--rust-completer",
                \ "java" : "--java-completer",
                \ "mono" : "--cs-completer"
                \ }
    for prog in keys(l:installSupports)
        if Executable(prog)
            call add(l:installOptions, l:installSupports[prog])
        endif
    endfor
    let l:installCmd="python3 install.py " . join(l:installOptions ," ") . " | tee $HOME/.logs/ycm_install.log 2>&1"
    exec '!' . l:installCmd
    if filereadable(PathExpand('third_party/ycmd/ycm_core.so'))
        let g:YCM_Installed=1
    else
        return 0
    endif
    call plug#load("YouCompleteMe")
endfunction

let s:YCM_dir=PathExpand(g:plug_home . '/' . s:name)
let g:YCMInstallOps={'do': ":call BuildYCM()", 'dir': s:YCM_dir, 'on': []}
Plugin 'https://github.com/ycm-core/YouCompleteMe', g:YCMInstallOps
unlet g:YCMInstallOps
if filereadable(PathExpand(s:YCM_dir . '/third_party/ycmd/ycm_core.so'))
    let g:YCM_Installed=1
endif
if exists("g:YCM_Installed")
    call LoadPluginOnInsertEnter('YouCompleteMe')
else
    finish
endif
