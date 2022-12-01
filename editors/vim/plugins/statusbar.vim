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


if IsPluginUsed("vim-airline")
    call SourcePluginFile("airline.vim")
endif
