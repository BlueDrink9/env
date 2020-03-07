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

" {[} Settings and dir creation
" Skip loading this file entirely.
if exists("g:noPlugins")
    let g:liteMode=0
    let g:ideMode=0
    finish
endif

" {[} ------ IDE Mappings ------
" gh - get hint on whatever's under the cursor
" Use g[] for get [something].
" Use <leader>i for ide bits.
" Use <leader>e for errors/linting/fixing.
let g:IDE_mappings = {
            \ "REPLSend" : "<leader>s",
            \ "REPLSendLine" : "<leader>ss",
            \ "REPLSendAndInsert" : "<leader>si",
            \ "REPLClear" : "<leader>sc",
            \ "make" : "<leader>im",
            \ "allActions" : "<leader>ia",
            \ "allCommands" : "<leader>ic",
            \ "codeAction" : "<leader>ia",
            \ "codeActionSelected" : "<leader>iaa",
            \ "codelensAction" : "<leader>ial",
            \ "complete" : "<plug>Non-existant",
            \ "definition" : "<leader>id",
            \ "definition2" : "gd",
            \ "documentation" : "K",
            \ "documentation2" : "gh",
            \ "documentation3" : "<leader>ih",
            \ "fix" : "<leader>ef",
            \ "implementation" : "<leader>ii",
            \ "implementation2" : "gi",
            \ "listErrs" : "<leader>el",
            \ "refactor" : "<leader>ir",
            \ "references" : "<leader>if",
            \ "references2" : "gr",
            \ "reformat" : "<plug>Non-existant",
            \ "rename" : "<leader>irr",
            \ "renameModule" : "<leader>irm",
            \ "snippet_expand" : "<c-e>",
            \ "snippet_prev" : "<c-b>",
            \ "snippet_next" : "<c-f>",
            \ "type_definition" : "gy",
            \ "debug_file" : "<leader>dd",
            \ "set_breakpoint" : "<leader>b",
            \}
" {]} ------ IDE Mappings ------


" Silence a python deprecation warning.
if has('python3') && !has('patch-8.1.201')
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

"
let g:proseFileTypes = "'latex,context,plaintex,tex,
            \markdown,mkd,
            \text,textile,
            \git,gitsendemail,
            \mail'"

if !filereadable(s:localPlugins)
    new
    silent exec 'write ' . s:localPlugins
    bdelete
endif

" To remove a Plugged repo using UnPlug 'user/repo'
function! s:deregister(repo)
    let repo = substitute(a:repo, '[\/]\+$', '', '')
    let name = fnamemodify(repo, ':t:s?\.git$??')
    call remove(g:plugs, name)
    call remove(g:plugs_order, index(g:plugs_order, name))
endfunction
command! -nargs=1 -bar UnPlug call s:deregister(<args>)

function! IsPluginUsed(name)
    if has_key(g:plugs, a:name)
        return 1
    else
        return 0
    endif
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

let g:plugindir = PathExpand(s:scriptdir . "/" . "plugins")
" Get light plugin set first
exec 'source ' . g:plugindir . "/colorschemes.vim"
exec 'source ' . g:plugindir . "/light_plugins.vim"
if !g:liteMode
    exec 'source ' . g:plugindir . "/main_plugins.vim"
    exec 'source ' . g:plugindir . "/statusbar.vim"
    if g:ideMode
        exec 'source ' . g:plugindir . "/ide_plugins.vim"
    endif
endif

" Unplugs and replacements go here
exec 'source ' . s:localPlugins

call plug#end()

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
