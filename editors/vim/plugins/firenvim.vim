if has('nvim') && has('node')
    " plugin firenvim in chrome and firefox.
    " Open textframes in nvim, similar to wasavi.
    Plug 'https://github.com/glacambre/firenvim', {'do': ':call firenvim#install(0)'}
    " Configured as json, basically.
    " Enable only on a few websites by default
    let g:firenvim_config = {
                \ 'localSettings': {
                \ '.*': {
                \ 'selector': 'textarea',
                \ 'priority': 0,
                \ },
                \ 'github\.com': {
                \ 'selector': 'textarea, * [contenteditable="true"]',
                \ 'priority': 1,
                \ },
                \ 'google\.com': {
                \ 'priority': 0,
                \ },
                \ },
                \ }
                " \ 'kaggle\.com': { " Regular kaggle is just for console.
                " \ 'priority': 0,
                " \ },
    function! FirenvimSetup(channel)
        let l:ui = nvim_get_chan_info(a:channel)
        if has_key(l:ui, 'client') &&
                    \ has_key(l:ui.client, "name") &&
                    \ l:ui.client.name == "Firenvim"
          " We are in firenvim
            let g:hasGUI=1
            let g:loaded_airline = 1
            let g:liteMode = 1
            silent! AirlineToggle
            call SetGFN(12)
            call add(g:customHLGroups, "EndOfBuffer guifg=guibg")
            " set nonumber
            " set norelativenumber
            set cmdheight=1
            set laststatus=0
            set noshowmode
            set termguicolors
            set noruler
            set noshowcmd
            set shortmess=aWAFtI
            " colorscheme PaperColor
            colorscheme github
            au! myVimrc FocusLost,InsertLeave,BufLeave * ++nested call Autosave()
            autocmd myPlugins BufNewFile *.txt call s:FirenvimSetFT()
        endif
    endfunction

    function! s:FirenvimSetFT()
        let l:bufname=expand('%:t')
        if l:bufname =~ "github.com"
            colorscheme github
            set ft=markdown
        elseif l:bufname =~ "cocalc.com" || l:bufname =~ "kaggleusercontent.com"
            set ft=python
        elseif l:bufname =~ "localhost"
            " Assume Jupyter notebook.
            set ft=python
        elseif l:bufname =~ "reddit.com"
            set ft=markdown
        elseif l:bufname =~ "stackexchange.com" || l:bufname =~ "stackoverflow.com"
            set ft=markdown
        endif
    endfunction

    autocmd myPlugins UIEnter * call s:FirenvimSetup(deepcopy(v:event.chan))
endif
