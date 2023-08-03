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

let s:pluginInstallPath = CreateVimDir("/pack")
let s:localPlugins = PathExpand(g:vimfilesDir . "/local_plugins.vim")
let s:scriptdir = expand('<sfile>:p:h')
let g:plugindir = PathExpand(s:scriptdir . "/" . "plugins")

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

" Silence a python deprecation warning.
if has('python3') && !(has('patch-8.1.201') || has('nvim')) && g:liteMode == 0
  silent! python3 1
endif

if has('win32') || has ('win64')
    let s:vimhome = $HOME."/vimfiles"
else
    let s:vimhome = $HOME."/.vim"
 endif

" Install plugin manager
if has('nvim')
    let s:jetpack_dir = stdpath('data') .. '/site/pack/jetpack/opt/vim-jetpack/plugin'
else
    let s:jetpack_dir = expand(s:vimhome) .. '/pack/jetpack/opt/vim-jetpack/plugin'
endif
let s:plugin_manager_file = s:jetpack_dir .. '/jetpack.vim'

let s:jetpackurl = "https://raw.githubusercontent.com/tani/vim-jetpack/master/plugin/jetpack.vim"
if !filereadable(s:plugin_manager_file)
    exec "silent !mkdir -p " . s:jetpack_dir
    if Executable("curl")
        let s:downloader = "curl -fLo "
    elseif Executable("wget")
        let s:downloader = "wget --no-check-certificate -O "
    else
        echoerr "You have to install curl or wget, or install jetpack yourself!"
        echoerr "jetpack not installed. No plugins will be loaded."
        finish
    endif
    " Continue installing...
    echom "Installing jetpack..."
    echo ""
    echom printf('%s %s %s', s:downloader, s:plugin_manager_file, s:jetpackurl)
    call system(printf('%s %s %s', s:downloader, s:plugin_manager_file, s:jetpackurl))
    if !filereadable(s:plugin_manager_file)
        echoerr "jetpack failed to install. No plugins will be loaded."
        finish
    endif
    autocmd myPlugins VimEnter * call jetpack#sync()
endif
packadd vim-jetpack



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

" To remove a Plugged repo using UnPlug 'pluginName'
function! s:deregister(name)
    return
    try
        call remove(g:plugs, a:name)
        call remove(g:plugs_order, index(g:plugs_order, a:name))
        " strip anything after period because not legitimate variable.
        let l:varname = substitute(a:name, '\..*', '', '')
        let l:varname = substitute(l:varname, 'vim-', '', '')
        exec 'let g:loaded_' . l:varname . ' = 1'
    catch /^Vim\%((\a\+)\)\=:E716:/
        echom 'Unplug failed for ' . a:name
    endtry
endfunction
command! -nargs=1 -bar UnPlug call s:deregister(<args>)

function! IsPluginUsed(name)
    return jetpack#tap(a:name)
endfunction

function! LoadPluginOnInsertEnter(name)
  call LoadPluginOnEvent(a:name, "InsertEnter")
endfunction

command! -bang -nargs=* Plug Jetpack <args>

" Plug installs the plugin, but only loads on autocmd event.
" name: the last part of the plugin url (just name, no auth).
" Plug options should include 'on': [] to prevent load before event.
function! LoadPluginOnEvent(name, event)
  let l:plugLoad = 'autocmd ' . a:event . ' * call jetpack#load("'
  let l:plugLoadEnd = '")'
  let l:undoAutocmd = 'autocmd! ' . a:name . '_' . a:event
  exec "augroup " . a:name . '_' . a:event
    autocmd!
    exec  l:plugLoad . a:name . l:plugLoadEnd . ' | ' . l:undoAutocmd
  augroup END
endfunction

cabbrev packi JetpackSync
cabbrev packu JetpackSync

let g:pluginSettingsToExec = []
let g:customHLGroups = []

" Reset plugins entirely
" let g:plugs={}
" let g:plugs_order=[]
" {]}
call jetpack#begin(s:pluginInstallPath)
Jetpack 'tani/vim-jetpack', {'opt': 1} "bootstrap

if has('nvim') && !has('nvim-0.9')
    Plug 'https://github.com/lewis6991/impatient.nvim'
endif

" For profiling startup time.
" Plug 'https://github.com/dstein64/vim-startuptime'
" let g:startuptime_tries = 3
" let g:ideMode=1

" Get light plugin set first
call SourcePluginFile("colorschemes.vim")
call SourcePluginFile("light_plugins.vim")
if !g:liteMode
    call SourcePluginFile("main_plugins.vim")
    if has('nvim-0.5')
        Plug 'https://github.com/nvim-lualine/lualine.nvim'
    else
        Plug 'https://github.com/vim-airline/vim-airline-themes'
        Plug 'https://github.com/vim-airline/vim-airline'
    endif
    if g:ideMode
        call SourcePluginFile("ide_plugins.vim")
    endif
endif

" Unplugs and replacements go here
exec 'source ' . s:localPlugins

call jetpack#end()

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
silent doautocmd User pluginSettingsToExec

" HLGroups get cleared by colourschemes when changing. This resets them.
function! s:reHL()
    for item in g:customHLGroups
        exec "highlight! " . item
    endfor
endfunction
call s:reHL()
autocmd myPlugins VimEnter,ColorScheme * call s:reHL()
