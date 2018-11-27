" vim: foldmethod=marker
" vim: foldmarker={[},{]}
"
if !exists ('colorSch')
    let colorSch="solarized"
endif

augroup colourschemes
    autocmd!
    autocmd colorscheme * let colorSch = get(g:, 'colors_name', 'default')
augroup end
" {[} ---------- Solarized ----------
if v:version >= 704 && has('termguicolors') && &termguicolors == 1
    Plug 'https://github.com/lifepillar/vim-solarized8'
    let g:solarized_old_cursor_style=1
    if $TERM =~ "screen" || $TERM =~ "tmux" " Tmux doesn't handle termguicols properly
        if g:colorSch =~ "solarized"
            let g:termColors = 16
            set t_Co=16
        endif
        set notermguicolors
    endif
    if g:termColors == 16
        let g:solarized_use16 = 1
    endif
    if g:colorSch =~ "solarized"
        call add (g:pluginSettingsToExec, "colorscheme solarized8_high")
    endif
else
    Plug 'https://github.com/altercation/vim-colors-solarized.git'
    " Settings doesn't recommend this...
    let g:solarized_contrast = "high"
    let g:solarized_termtrans = 0 " 1 displays default term bg instead.
    let g:solarized_menu = 0
    if g:termColors == 16
        " According to solarized help, 16 is default anyway, so shouldn't need
        " these set if not using 256.
        " let g:solarized_base16 = 1
        let g:solarized_termcolors=16
    elseif g:termColors == 256
        let g:solarized_termcolors=256
    endif
    call add (g:pluginSettingsToExec, "colorscheme " . colorSch)
endif
" {]}

Plug 'https://github.com/vim-scripts/oceandeep'
Plug 'https://github.com/morhetz/gruvbox'
Plug 'https://github.com/tomasr/molokai'

" call add (g:pluginSettingsToExec, "colorscheme " . colorSch)
call add (g:customHLGroups, "MatchParen cterm=bold,underline ctermbg=lightgray")
call add (g:customHLGroups, "MatchParen gui=bold,underline guibg=gray90")
" call add (g:customHLGroups, "link MatchParen CursorColumn")
call add (g:customHLGroups, "clear SignColumn")
call add (g:customHLGroups, "link SignColumn LineNr")
