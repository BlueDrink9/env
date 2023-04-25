" vim: foldmethod=marker
" vim: foldmarker={[},{]}
"
augroup myColourschemes
    autocmd!
augroup end

autocmd myColourschemes colorscheme * let colorSch =
            \ get(g:, 'colors_name', 'default')

function! GetBackground()
    return synIDattr(synIDtrans(hlID('SignColumn')), 'bg')
endfunction

let s:guiColours = v:false
silent! if &termguicolors || g:hasGUI
    let s:guiColours = v:true
endif

Plug 'reedes/vim-colors-pencil'
let g:pencil_higher_contrast_ui = 0   " 0=low (def), 1=high
let g:pencil_terminal_italics = 1
let g:pencil_spell_undercurl = 1
" Very similar to the pencil theme. Pencil doesn't support terminal
" undercurl.
Plug 'NLKNguyen/papercolor-theme'
let g:PaperColor_Theme_Options = {
      \   'theme': {
      \     'default': {
      \       'allow_bold': '1',
      \       'allow_italic': '1',
      \     }
      \   }
      \ }
" \       'override' : {
" \         'SpellBad' : ['underline', 'underline'],
" \         'SpellCap' : ['underline', 'underline']
" \       }
if has('nvim')
  Plug 'https://github.com/ellisonleao/gruvbox.nvim'
else
  Plug 'https://github.com/morhetz/gruvbox'
endif
Plug 'https://github.com/jnurmine/Zenburn'
" CursorColumn a stronger option
" au myPlugins colorscheme zenburn hi! link StatusLineNC ColorColumn | hi! link StatusLine ColorColumn
Plug 'https://github.com/tomasr/molokai'
Plug 'aonemd/kuroi.vim'
" Gorgeously coloured dark scheme.
Plug 'https://github.com/srcery-colors/srcery-vim'
let g:srcery_italic=1 " Default only 1 in gui
Plug 'https://github.com/rakr/vim-two-firewatch'
if s:guiColours
  if has('nvim')
    Plug 'https://github.com/Shatur/neovim-ayu'
  else
    Plug 'https://github.com/ayu-theme/ayu-vim'
    " Apparently we can't have all versions. Daft.
    " Dark version doesn't work easily with airline anyway.
    let ayucolor="light"
  endif
endif
Plug 'liuchengxu/space-vim-dark'
" High contrast themes.
" Mainly for rift (dark green) and maybe day (sol-style), summer (dark
" purple). Past (pale light). Spring is also green.
Plug 'https://gitlab.com/protesilaos/tempus-themes-vim'
" Green-ish?
Plug 'https://github.com/vim-scripts/oceandeep'
" Purple can be quite fun.
Plug 'ashfinal/vim-colors-violet'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'https://github.com/pineapplegiant/spaceduck'
Plug 'https://github.com/sainnhe/forest-night'

if has('nvim')
  Plug 'https://github.com/uloco/bluloco.nvim'
endif

Plug 'sonph/onehalf', {'rtp': 'vim/'}
function! s:onehalfSwapLightDark()
  if g:colorSch =~? "dark"
    colorscheme onehalflight
  else
    colorscheme onehalfdark
  endif
endfunction
function! s:setOnehalfColourSwapAutocmd()
  au myColourschemes OptionSet background if g:colorSch =~? "onehalf" | call <sid>onehalfSwapLightDark() | endif
endfunction
" When using onehalf, set up an autocmd to change colourscheme when
" 'background' option is set (e.g. with `yob`)
au myPlugins ColorScheme onehalf* call <sid>setOnehalfColourSwapAutocmd() | au! myColourschemes ColorScheme onehalf*

Plug 'rakr/vim-one'
let g:one_allow_italics = 1
function! s:customVimOneColours()
  " Don't hide window borders. May want to play with colours (fg, bg).
  call one#highlight('vimLineComment', 'cccccc', '', 'none')
  " Fix italics and remove colouring from bolded/italic text.
  " hi clear markdownItalic | hi link markdownItalic htmlItalic
  " hi clear markdownBold | hi link markdownBold htmlBold
  call one#highlight('markdownItalic', '', '', 'italic')
  call one#highlight('markdownBoldItalic', '', '', 'italic,bold')
endfunction
autocmd myColourschemes ColorScheme one call <sid>customVimOneColours()
Plug 'cormacrelf/vim-colors-github'
Plug 'https://github.com/arzg/vim-colors-xcode'
Plug 'https://github.com/mvpopuk/inspired-github.vim'
" Black backgrounds
Plug 'fcpg/vim-fahrenheit'
Plug 'vim-scripts/Luinnar'
Plug 'https://github.com/GertjanReynaert/cobalt2-vim-theme'
Plug 'nanotech/jellybeans.vim'
let g:jellybeans_use_term_italics = 1
let g:jellybeans_overrides = {
      \    'background': { 'guibg': '000000' },
      \}

" {[} ---------- Base16 ----------
" If using a Base16 terminal theme designed to keep the 16 ANSI colors intact (a "256" variation) and have sucessfully modified your 256 colorspace with base16-shell you'll need to add the following to your ~/.vimrc before the colorscheme declaration.
" Should override COLOURSCHEME settings by setting colorSch
" let base16colorspace=256  " Access colors present in 256 colorspace
if !s:guiColours
    Plug 'https://github.com/chriskempson/base16-vim'
    if filereadable(expand("~/.vimrc_background")) && exists($BASE16_THEME)
        let base16colorspace=256
        autocmd myPlugins User pluginSettingsToExec source ~/.vimrc_background
        autocmd myPlugins User pluginSettingsToExec let colorSch=g:colors_name
    endif
endif
" {]} ---------- Base16 ----------

" {[} ---------- Solarized ----------
" Altercation's version doesn't set gui colors in terminal (no termguicolours)
if has("nvim") && &termguicolors == 1
    Plug 'https://gitlab.com/HiPhish/resolarized.nvim'
elseif exists("g:gui_oni")
    if colorSch =~ "solarized"
        " Comes with oni, includes theme.
        let colorSch = "solarized8"
    endif
elseif &termguicolors == 1
    Plug 'https://github.com/lifepillar/vim-solarized8'
    if colorSch =~ "solarized"
        " Comes with oni, includes theme.
        let colorSch = "solarized8"
    endif
else
    Plug 'bluedrink9/vim-colors-solarized'
    " Settings doesn't recommend this...
    let g:solarized_contrast = "high"
endif
let g:solarized_visibility='highv:'
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

" {]}

function! BlackenBackgroundColours()
  let l:override_groups = ["Normal",
        \ "airline_c","airline_c_bold", "airline_c1_inactive",
        \ "airline_x","airline_x_bold", "airline_x1_inactive",
        \ "airline_tab",
        \ "airline_tabmod", "airline_tabmod_unsel",
        \ "airline_tabfill", "airline_tabsel", "airline_tablabel",
        \ "airline_tabhid",
        \ "airline_tabfill_to_airline_tabfill",
        \ "airline_tabsel_to_airline_tabfill",
        \ "airline_tab_right",
        \ "airline_tabsel_right",
        \ "airline_tab_left",
        \ "airline_tabsel_left",
        \ "airline_tabtype",
        \]
  for g in l:override_groups
    call add (g:customHLGroups, g . " ctermbg=black guibg=black")
  endfor
  call add (g:customHLGroups, "airline_tabsel cterm=bold gui=bold")
endfunction

" Usually using termux on amoled screens, so want pure black bg.
if $ISTERMUXSCREEN
  let colorSch = "srcery"
  call BlackenBackgroundColours()
endif

" exec so variables are set at the right time (after plugin load).
autocmd myPlugins User pluginSettingsToExec ++nested exec 'colorscheme ' . colorSch
" These are old fixes, so I think I probably just don't need them on nvim
if !has('nvim')
    call add (g:customHLGroups, "MatchParen cterm=bold,underline ctermbg=lightgray")
    call add (g:customHLGroups, "MatchParen gui=bold,underline guibg=gray90")
    " call add (g:customHLGroups, "link MatchParen CursorColumn")
    " call add (g:customHLGroups, "clear SignColumn")
    call add (g:customHLGroups, "link SignColumn LineNr")
endif
if has('nvim')
    " Group used by neovim for listchars
    call add (g:customHLGroups, "link Whitespace Comment")
endif

" Hide the built-in schemes
set wildignore+=blue.vim,darkblue.vim,delek.vim,desert.vim,
      \elflord.vim,evening.vim,industry.vim,koehler.vim,morning.vim,murphy.vim,
      \pablo.vim,peachpuff.vim,ron.vim,shine.vim,slate.vim,torte.vim,zellner.vim

