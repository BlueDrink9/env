if !exists("IsPluginUsed")
    function! IsPluginUsed()
        return false
    endfunction
endif

function! RMakeHTML()
    update
    call s:RSetWD()
    let l:filename = expand("%:r:t")
    " Rscript -e 'library(knitr);knit()'
    let l:rcmd = "require('knitrBootstrap');
                \knit_bootstrap(\"" . l:filename . ".Rmd\")"
    if g:vimrplugin_openhtml
        let l:rcmd = l:rcmd . '; browseURL("' . l:filename . '.html")'
    endif
    call s:sendCmdToR(l:rcmd)
endfunction
nnoremap <silent> <buffer> <Leader>ll :call RMakeHTML()<CR>

function! s:sendCmdToR(rcmd)
    if IsPluginUsed("vim-r-plugin")
        " From vim-r-plugin
        call SendCmdToR(a:rcmd)
    else
        !Rscript -e a:rcmd
    endif
endfunction

function! s:RSetWD()
    if IsPluginUsed("vim-r-plugin")
        " From vim-r-plugin
        call RSetWD()
    else
        !Rscript -e "setwd('%:h')"
    endif
endfunction

" if !IsPluginUsed("vim-r-plugin")
"     function! RSetWD()
"     endfunction
" endif
