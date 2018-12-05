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

Plug 'https://github.com/vim-scripts/oceandeep'
Plug 'https://github.com/morhetz/gruvbox'
Plug 'https://github.com/tomasr/molokai'
" {[} ---------- Base16 ----------
" If using a Base16 terminal theme designed to keep the 16 ANSI colors intact (a "256" variation) and have sucessfully modified your 256 colorspace with base16-shell you'll need to add the following to your ~/.vimrc before the colorsheme declaration.
" let base16colorspace=256  " Access colors present in 256 colorspace
Plug 'https://github.com/chriskempson/base16-vim'
" {]} ---------- Base16 ----------

" {[} ---------- Solarized ----------
if v:version >= 704 && has('termguicolors') && &termguicolors == 1
    Plug 'https://github.com/lifepillar/vim-solarized8'
    let g:solarized_old_cursor_style=1
    if $TERM =~ "screen" || $TERM =~ "tmux" " Tmux doesn't handle termguicols properly
        set notermguicolors
        if g:colorSch =~ "solarized"
            let g:termColors = 16
            set t_Co=16
        endif
    endif
    if g:termColors == 16
        let g:solarized_use16 = 1
    endif
    if g:colorSch =~ "solarized"
        call add (g:pluginSettingsToExec, "colorscheme solarized8_high")
    endif
else
    if $ISTERMUX
        " Replaces solarized for termux solarized theme
        https://github.com/Breta01/Termux-Themes
    else
        Plug 'https://github.com/altercation/vim-colors-solarized.git'
    endif
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

" call add (g:pluginSettingsToExec, "colorscheme " . colorSch)
call add (g:customHLGroups, "MatchParen cterm=bold,underline ctermbg=lightgray")
call add (g:customHLGroups, "MatchParen gui=bold,underline guibg=gray90")
" call add (g:customHLGroups, "link MatchParen CursorColumn")
call add (g:customHLGroups, "clear SignColumn")
call add (g:customHLGroups, "link SignColumn LineNr")
