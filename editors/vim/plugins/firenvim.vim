" Needs nvim > 0.4, which was probably also when UIEnter was introduced.
if has('nvim') && exists('##UIEnter')
    " plugin firenvim in chrome and firefox.
    " Open textframes in nvim, similar to wasavi.
    let s:startup_prologue='"export LITE_SYSTEM=1"'
    let g:firenvim_install=":call firenvim#install(0, " . s:startup_prologue . ")"
    Plug 'https://github.com/glacambre/firenvim', {'do': g:firenvim_install}
    " Configured as json, basically.
    " disable by default. Manually activate with chrome binding.
    let g:firenvim_config = {
        \ 'localSettings': {
            \ '.*': {
                \ 'selector': 'textarea, div[role="textbox"]',
                \ 'priority': 0,
                \ 'takeover': 'once',
            \ }
        \ }
        \ }
    function! s:FirenvimSetup(channel)
        let l:ui = nvim_get_chan_info(a:channel)
        if !(has_key(l:ui, 'client') &&
                    \ has_key(l:ui.client, 'name') &&
                    \ l:ui.client.name ==# 'Firenvim')
            return
        endif
      " We are in firenvim
        let g:hasGUI=1
        let g:liteMode = 1
        call SetGFN(12)
        set termguicolors
        call add(g:customHLGroups, 'EndOfBuffer guifg=guibg')
        " colorscheme PaperColor
        colorscheme github
        set colorcolumn=0
        if &lines < 18
            let g:loaded_airline = 1
            silent! AirlineToggle
            " See neovim #1004
            " set cmdheight=0
            set cmdheight=1
            set laststatus=0
            set noshowmode
            set noruler
            set noshowcmd
            set shortmess=aWAFtI
            " Can't afford to hard wrap by mistake.
            set textwidth=200
        endif
        if &columns < 15
            set nonumber
            set norelativenumber
        endif

        nnoremap <C-z> :call firenvim#focus_page()<cr>
        nnoremap <Esc><Esc><Esc> :call firenvim#focus_page()<CR>
        au! myVimrc FocusLost,InsertLeave,BufLeave * ++nested call Autosave()
        autocmd myPlugins BufNewFile *.txt call s:FirenvimSetPageOptions()
    endfunction

    function! s:FirenvimSetPageOptions()
        let l:bufname=expand('%:t')
        if l:bufname =~? 'github.com'
            colorscheme github
            set ft=markdown
        elseif l:bufname =~? 'cocalc.com' || l:bufname =~? 'kaggleusercontent.com'
            set ft=python
        elseif l:bufname =~? 'localhost'
            " Assume Jupyter notebook.
            set ft=python
        elseif l:bufname =~? 'reddit.com'
            set ft=markdown
        elseif l:bufname =~? 'stackexchange.com' || l:bufname =~? 'stackoverflow.com'
            set ft=markdown
        elseif l:bufname =~? 'slack.com' || l:bufname =~? 'gitter.com'
            " for chat apps. Enter sents the message and deletes the buffer.
            " Shift enter is normal return. Insert mode by default.
            normal! i
            inoremap <CR> <Esc>:w<CR>:call firenvim#press_keys("<LT>CR>")<CR>ggdGa
            inoremap <s-CR> <CR>
        endif
    endfunction

    autocmd myPlugins UIEnter * call s:FirenvimSetup(deepcopy(v:event.chan))
endif
