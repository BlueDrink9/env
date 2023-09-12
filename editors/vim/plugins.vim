" vim: set ft=vim:
" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" This file will source from light, main and ide plugin files, depending on
" the value of 2 variables. These should be set from command line via the
" --cmd option, eg $(vim --cmd "let g:liteMode=1" [file])

augroup myPlugins
    au!
augroup end

" These should speed up things quite a lot, so specify them in .vimrc if you
" can.
" let g:python_host_prog  = '/usr/bin/python2'
" let g:python3_host_prog = '/usr/bin/python3'
" let g:node_host_prog = '/usr/bin/node'

" {[} ------ Disable built-in plugins ------
" " Disable vim distribution plugins
" let g:loaded_getscript = 1
" let g:loaded_getscriptPlugin = 1
" let g:loaded_gzip = 1
" let g:loaded_logiPat = 1
" let g:loaded_matchit = 1
" let g:loaded_matchparen = 1
" let g:netrw_nogx = 1 " disable netrw's gx mapping.
" let g:loaded_rrhelper = 1  " ?
" let g:loaded_shada_plugin = 1  " ?
" let g:loaded_tar = 1
" let g:loaded_tarPlugin = 1
" let g:loaded_tutor_mode_plugin = 1
" let g:loaded_2html_plugin = 1
" let g:loaded_vimball = 1
" let g:loaded_vimballPlugin = 1
" let g:loaded_zip = 1
" let g:loaded_zipPlugin = 1
" {]} ------ Disable built-in plugins ------

" {[} Settings and dir creation

let g:plugindir = PathExpand(g:configDir . "/" . "plugins")

function! SourcePluginFile(name)
    exec 'source ' . g:plugindir . '/' . a:name
endfunction

" Skip loading this file entirely.
if exists("g:noPlugins")
    let g:liteMode=0
    let g:ideMode=0
    call SourcePluginFile("noplugin_alts.vim")
    finish
endif

let g:pluginInstallPath = CreateVimDir("/plugins")
let s:localPlugins = PathExpand(g:vimfilesDir . "/local_plugins.vim")

call SourceCustoms("manage_plugins.vim")

" Silence a python deprecation warning.
if has('python3') && !(has('patch-8.1.201') || has('nvim')) && g:liteMode == 0
  silent! python3 1
endif

 if has('nvim')
    cabbrev packi lua require('config.lazy') require("lazy").install(MyLazySpecs)
    cabbrev packu Lazy check <bar> Lazy update
 else
    cabbrev packi PlugInstall
    cabbrev packu PlugUpdate <bar> PlugUpgrade
endif


let g:proseFileTypes = ["latex","context","plaintex","tex","rnoweb",
            \"markdown","mkd","rmd",
            \"text","textile",
            \"git","gitsendemail",
            \"mail",
            \]

if !filereadable(s:localPlugins)
    new
    silent exec 'write ' . s:localPlugins
    bdelete
endif

let g:pluginSettingsToExec = []
let g:customHLGroups = []

" Reset plugins entirely
" let g:plugs={}
" let g:plugs_order=[]
" {]}
if !has('nvim')
    call plug#begin(g:pluginInstallPath)
endif
" Jetpack 'tani/vim-jetpack', {'opt': 1} "bootstrap

if has('nvim') && !has('nvim-0.9')
    Plugin 'https://github.com/lewis6991/impatient.nvim'
endif

" For profiling startup time.
" Plugin 'https://github.com/dstein64/vim-startuptime'
" let g:startuptime_tries = 3
" let g:ideMode=1

" Get light plugin set first
call SourcePluginFile("colorschemes.vim")
call SourcePluginFile("light_plugins.vim")
if !g:liteMode
    call SourcePluginFile("main_plugins.vim")
    if !has('nvim-0.5')
        Plugin 'https://github.com/vim-airline/vim-airline-themes'
        Plugin 'https://github.com/vim-airline/vim-airline'
    endif
    if g:ideMode
        call SourcePluginFile("ide_plugins.vim")
    endif
endif

" Unplugs and replacements go here
exec 'source ' . s:localPlugins

if !has('nvim')
    call plug#end()
endif

if has('nvim') && !has('nvim-0.9')
    lua require('impatient')
endif

call SourcePluginFile("light_config.vim")
if !g:liteMode
    " Check if we can use powerline/nerd font symbols.
    " Sets g:useNerdFont || g:usePLFont
    call SourcePluginFile("symbol_check.vim")
    call SourcePluginFile("main_config.vim")
    call SourcePluginFile("statusbar.vim")
    if g:ideMode
      call SourcePluginFile("ide_config.vim")
    endif
endif

" Inits lazy.nvim plugin loading
if has('nvim')
    lua require("config.lazy")
endif
silent doautocmd User pluginSettingsToExec

" HLGroups get cleared by colourschemes when changing. This resets them.
function! s:reHL()
    for item in g:customHLGroups
        exec "highlight! " . item
    endfor
endfunction
call s:reHL()
autocmd myPlugins VimEnter,ColorScheme * call s:reHL()
