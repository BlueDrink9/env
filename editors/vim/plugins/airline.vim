
" {[} ---------- Airline ----------

if IsPluginUsed("asyncrun.vim")
    " Async errors appear in airline.
    let g:asyncrun_status = ''
    autocmd myPlugins User pluginSettingsToExec let g:airline_section_error = airline#section#create_right(['%{g:asyncrun_status}'])
endif

function! s:AirlineColorVarUpdate()
    let s:restoreCS = g:colorSch
    " All airline themes are lowercase, but not all theme names are. For
    " example, PaperColor.
    let g:colorSch= tolower(g:colorSch)
    if g:colorSch =~ "base16"
        " Strips off the 'base16-' bit.
        " let g:colorSch = g:colorSch[7:]
        " Only takes the second bit between hyphens.
        " let g:colorSch = split(g:colorSch,"-")[1]
        let g:colorSch = "base16"
    elseif g:colorSch =~? "solarized"
        " Covers solarized variants like solarized8, neosolarized, etc.
        " After base16 so it doesn't catch base16-solarized-*.
        let g:colorSch = "solarized"
    endif
    " Any schemes not defined for airline
    if exists ('*airline#util#themes()') &&
                \ (index(airline#util#themes(g:colorSch), g:colorSch) == -1)
        let g:colorSch = "default"
    endif
    if g:colorSch == "default"
        " let g:colorSch = &background
        " If theme is unset, Airline will pick from theme highlight colours to
        " get a nice match.
        if exists('g:airline_theme')
            unlet g:airline_theme
        endif
    else
        let g:airline_theme=g:colorSch
    endif
    exec 'let g:airline_' . g:colorSch . '_bg="' . &background . '"'
    let g:colorSch=s:restoreCS
endfunction

call s:AirlineColorVarUpdate()
let g:airline_highlighting_cache = 1
let g:airline#extensions#wordcount#enabled = 1
let g:airline_solarized_normal_green = 1
let g:airline_solarized_dark_inactive_border = 1
" exec 'let g:airline_base16_' . colorSch . '= 0'
" {[} ---------- Nerd/pl fonts and symbols ----------
" let g:airline_symbols_ascii=1
" If ssh, don't assume font avail? Or should we...?
if g:remoteSession
    let g:airline_powerline_fonts=0
    let g:webdevicons_enable=0
endif
" Env variables, can be set by ssh client on login if it supports PL.
if $USENF==1
    let g:airline_powerline_fonts=1
    let g:webdevicons_enable=1
elseif $USEPF==1
    let g:airline_powerline_fonts=1
elseif $USEPF==0
    let g:airline_powerline_fonts=0
elseif $USENF==0
    let g:airline_powerline_fonts=0
    let g:webdevicons_enable=0
endif
" Check if either of these have been specifically disabled or enabled.
" airline_powerline gets set by default.
if !exists('g:webdevicons_enable')
" if !exists('g:airline_powerline_fonts') || !exists('g:webdevicons_enable')
    " Automatically check for powerline compatible font installed locally
    " (unix) or to system (windows)
    " If we are (probably) using a powerline compatible font, set it so.
    " If a nerd font is found, assume powerline-compat, as well as devicons.

    " Exists by default :/
    " if !exists('g:airline_powerline_fonts')
    if g:usePLFont
        let g:airline_powerline_fonts = 1
    else
        let g:airline_powerline_fonts = 0
    endif
    " endif

    if g:useNerdFont == 0
        if !exists('g:webdevicons_enable')
            " disable devicons and dependents.
            let g:NERDTreeDisableFileExtensionHighlight = 1
            let g:NERDTreeDisableExactMatchHighlight = 1
            let g:NERDTreeDisablePatternMatchHighlight = 1
            let g:webdevicons_enable = 0
        endif
    endif
endif

" if exists('g:airline_powerline_fonts') && g:airline_powerline_fonts == 0
" Should always exist, because if it doesn't it's created above.
if g:airline_powerline_fonts == 0 
    if !exists('g:airline_symbols')
        let g:airline_symbols = {}
    endif
    " unicode symbols
    let g:airline_symbols.linenr = '☰'
    let g:airline_symbols.maxlinenr = ''
    let g:airline_symbols.columnnr = '∥'
    let g:airline_symbols.branch = '⎇'
    let g:airline_symbols.paste = 'PASTE'
    let g:airline_symbols.whitespace = '☲'
    " airline symbols
    let g:airline_symbols.readonly = ''

    function! s:makeAirlineCustomParts()
        " Skip gap between col symbol and number (custom section)
        call airline#parts#define_raw('linenr', g:airline_symbols.linenr . ' %l')
        call airline#parts#define_raw('columnnr', g:airline_symbols.columnnr . '%c')
        let g:airline_section_z = airline#section#create([
                    \ 'linenr', 'maxlinenr',' ', 'columnnr'])
    endfunction

else
    " Using predefined symbols
    " This causes linenr to be blank. But without it, symbols don't exist!
    " There's no winning some times.
    " if !exists('g:airline_symbols')
    "     let g:airline_symbols = {}
    "     let g:airline_symbols.linenr = '∥'
    " endif
    function! s:makeAirlineCustomParts()
        let g:airline_symbols.columnnr = '∥'
        let g:airline_symbols.maxlinenr = ''
        call airline#parts#define_raw('columnnr', g:airline_symbols.columnnr . '%c')
        call airline#parts#define_raw('linenr', g:airline_symbols.linenr . '%l')
        " call airline#parts#define_raw('maxlinenr', '/%L' . g:airline_symbols.maxlinenr)
        call airline#parts#define_raw('maxlinenr', '/%L')
        " Skip gap between col symbol and number (custom section)
        let g:airline_section_z = airline#section#create([
                    \ 'linenr', 'maxlinenr',' ', 'columnnr'])
    endfunction
endif
autocmd myPlugins User pluginSettingsToExec call s:makeAirlineCustomParts()

" {]} ---------- Nerd/pl fonts----------

let g:airline#extensions#syntastic#stl_format_err="%E{Err: #%e L%fe}"
let g:airline#extensions#syntastic#stl_format_warn='%W{Warn: #%w L%fw}'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
let g:airline#extensions#tabline#show_buffers = 1
let g:airline#extensions#tabline#show_tabs = 0
" let g:airline#extensions#tabline#buffer_min_count = 0
" let g:airline#extensions#tabline#tab_min_count = 0
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#buffers_label = 'B'
let g:airline#extensions#whitespace#mixed_indent_algo = 1
" Alt sep gives b<c>b instead of b>c>b
let g:airline#extensions#tabline#alt_sep = 1
let g:airline#extensions#tabline# = 1
" let g:airline#extensions#tabline#show_tabs = 0
autocmd myPlugins User pluginSettingsToExec let g:airline_section_tabline = airline#section#create(['%{getcwd()}'])

let g:airline#extensions#hunks#non_zero_only = 1
let g:airline#extensions#whitespace#checks = []
" Disable mode shown in cmdline
set noshowmode
"  reduce delay on insert leaave?
set ttimeoutlen=50

augroup myAirline
    autocmd!
    autocmd colorscheme * call s:AirlineColorVarUpdate()
    if v:version >= 800
        autocmd optionset background call s:AirlineColorVarUpdate()
    endif
augroup end
" augroup myAirline
"     autocmd!
"     autocmd colorscheme let g:airline_theme=colorSch | AirlineRefresh
"     autocmd optionset background exec 'let g:airline_' . colorSch . '_bg="' . &background . '"' | AirlineRefresh
" augroup end

" {]} ---------- airline ----------
