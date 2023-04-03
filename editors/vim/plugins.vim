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
" Skip loading this file entirely.
if exists("g:noPlugins")
    let g:liteMode=0
    let g:ideMode=0
    finish
endif

" Silence a python deprecation warning.
if has('python3') && !(has('patch-8.1.201') || has('nvim')) && g:liteMode == 0
  echom 'python'
  silent! python3 1
endif

let s:pluginPath = CreateVimDir("/plugins")
let s:scriptdir = PathExpand('<sfile>:p:h')
let s:localPlugins = PathExpand(g:vimfilesDir . "/local_plugins.vim")
let s:scriptdir = expand('<sfile>:p:h')

if has('win32') || has ('win64')
    let $VIMHOME = $HOME."/vimfiles"
else
    let $VIMHOME = $HOME."/.vim"
endif
let s:autoloadDir=expand($VIMHOME . "/autoload")
let s:vimplug_file=expand(s:autoloadDir . "/plug.vim")
if !filereadable(s:vimplug_file)
    if executable("curl")
        let s:downloader = "!curl -fLo "
    elseif executable("wget")
        let s:downloader = "!wget --no-check-certificate -O "
    else
        echoerr "You have to install curl or wget, or install vim-plug yourself!"
        echoerr "vim-plug not installed. No plugins will be loaded."
        finish
    endif
    " Continue installing...
    exec "silent !mkdir -p " . s:autoloadDir
    echom "Installing Vim-Plug..."
    echo ""
    exec s:downloader . s:vimplug_file . " https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    if !filereadable(s:vimplug_file)
        echoerr "vim-plug failed to install. No plugins will be loaded."
        finish
    endif
    "   let g:not_finish_vimplug = "yes"
    autocmd myPlugins VimEnter * PlugInstall
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

let g:plugindir = PathExpand(s:scriptdir . "/" . "plugins")
function! SourcePluginFile(name)
    exec 'source ' . g:plugindir . '/' . a:name
endfunction

" To remove a Plugged repo using UnPlug 'pluginName'
function! s:deregister(name)
  try
    call remove(g:plugs, a:name)
    call remove(g:plugs_order, index(g:plugs_order, a:name))
    " strip anything after period because not legitimate variable.
    let l:varname = substitute(a:name, '\..*', '', '')
    let l:varname = substitute(l:varname, 'vim-', '', '')
    exec 'let g:loaded_' . l:varname . ' = 1'
  catch E716
    echom 'Unplug failed for ' . a:name
  endtry
endfunction
command! -nargs=1 -bar UnPlug call s:deregister(<args>)

function! IsPluginUsed(name)
    return has_key(g:plugs, a:name)
endfunction

function! LoadPluginOnInsertEnter(name)
  call LoadPluginOnEvent(a:name, "InsertEnter")
endfunction

" Plug installs the plugin, but only loads on autocmd event.
" name: the last part of the plugin url (just name, no auth).
" Plug options should include 'on': [] to prevent load before event.
function! LoadPluginOnEvent(name, event)
  let l:plugLoad = 'autocmd ' . a:event . ' * call plug#load("'
  let l:plugLoadEnd = '")'
  let l:undoAutocmd = 'autocmd! ' . a:name . '_' . a:event
  exec "augroup " . a:name . '_' . a:event
    autocmd!
    exec  l:plugLoad . a:name . l:plugLoadEnd . ' | ' . l:undoAutocmd
  augroup END
endfunction

cabbrev packi PlugInstall
cabbrev packu PlugUpdate <bar> PlugUpgrade

let g:pluginSettingsToExec = []
let g:customHLGroups = []

" Reset plugins entirely
" let g:plugs={}
" let g:plugs_order=[]
" {]}
call plug#begin(s:pluginPath)

if has('nvim')
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

call plug#end()

if has('nvim')
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

if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
    if !g:hasGUI
        echom "Some plugins defined in config are uninstalled"
    endif
endif
