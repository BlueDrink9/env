" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" Skip airline with
" let g:loaded_airline = 1
if exists("g:gui_oni")
    " if OniCommand('Oni.configuration.getValue("oni.statusbar.enabled")') == "true"
        set noshowmode
        set noruler
        set laststatus=0
        set noshowcmd
        finish
    " endif
endif

function! ColorschemeToAirlineTheme(colo_name)
    " All airline themes are lowercase, but not all theme names are. For
    " example, PaperColor.
    let l:colorSch= tolower(a:colo_name)
    if l:colorSch =~ "base16"
        " Strips off the 'base16-' bit.
        " let l:colorSch = l:colorSch[7:]
        " Only takes the second bit between hyphens.
        " let l:colorSch = split(l:colorSch,"-")[1]
        let l:colorSch = "base16"
    elseif l:colorSch =~? "solarized"
        " Covers solarized variants like solarized8, neosolarized, etc.
        " After base16 so it doesn't catch base16-solarized-*.
        let l:colorSch = "solarized"
    endif
    " Any schemes not defined for airline
    if exists ('*airline#util#themes()') &&
                \ (index(airline#util#themes(l:colorSch), l:colorSch) == -1)
        let l:colorSch = "default"
    endif
    return l:colorSch
endfunction

if IsPluginUsed("vim-airline")
    call SourcePluginFile("airline.vim")
endif

if IsPluginUsed("lualine.nvim")
    call SourcePluginFile("lualine.lua")
endif
