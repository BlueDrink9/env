if has('win32') || has ('win64')
    let s:vimhome = $HOME."/vimfiles"
else
    let s:vimhome = $HOME."/.vim"
 endif

augroup plugAdapter
  au!
augroup END


" Install plugin manager
" if has('nvim')
"     let s:plugin_manager_dir = stdpath('data') .. '/site/pack/jetpack/opt/vim-jetpack/plugin'
" else
"     let s:plugin_manager_dir = expand(s:vimhome) .. '/pack/jetpack/opt/vim-jetpack/plugin'
" endif
let s:plugin_manager_dir = expand(s:vimhome .. '/autoload')
let s:plugin_manager_file = s:plugin_manager_dir .. '/plug.vim'

" vim-plug specific
if !has('nvim')
" Jetpack
" let s:plugin_manager_url = "https://raw.githubusercontent.com/tani/vim-jetpack/master/plugin/jetpack.vim"
let s:plugin_manager_url = "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

if !filereadable(s:plugin_manager_file)
    exec "silent !mkdir -p " . s:plugin_manager_dir
    if Executable("curl")
        let s:downloader = "curl -fLo "
    elseif Executable("wget")
        let s:downloader = "wget --no-check-certificate -O "
    else
        echoerr "You have to install curl or wget, or install plugin manager yourself!"
        echoerr "Plugin manager not installed. No plugins will be loaded."
        finish
    endif
    " Continue installing...
    echom "Installing plugin manager..."
    echo ""
    call system(printf('%s %s %s', s:downloader, s:plugin_manager_file, s:plugin_manager_url))
    if !filereadable(s:plugin_manager_file)
        echoerr "Plugin manager not installed. No plugins will be loaded."
        finish
    endif
    autocmd myPlugins VimEnter * PlugInstall
endif
" packadd vim-jetpack

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

endif

if has('nvim')
    lua IsPluginUsed = function(name) return require("lazy.core.config").plugins[name] ~= nil end
endif
function! IsPluginUsed(name)
    if has('nvim')
        return has_key(s:plugs, a:name)
    else
        return has_key(g:plugs, a:name)
    endif
endfunction

" To remove a Plugged repo using UnPlug 'pluginName'
function! s:deregister(name)
    return
    try
        if has('nvim')
            call remove(s:plugs, a:name)
        else
            call remove(g:plugs, a:name)
            call remove(g:plugs_order, index(g:plugs_order, a:name))
        endif
        " strip anything after period because not legitimate variable.
        let l:varname = substitute(a:name, '\..*', '', '')
        let l:varname = substitute(l:varname, 'vim-', '', '')
        exec 'let g:loaded_' . l:varname . ' = 1'
    catch /^Vim\%((\a\+)\)\=:E716:/
        echom 'Unplug failed for ' . a:name
    endtry
endfunction
command! -nargs=1 -bar UnPlug call s:deregister(<args>)

if has('nvim')
let s:plugs = {}
lua << EOF
MyLazySpecs = {}
-- Compatibility function to convert vim-plug's Plug command to lazy.nvim spec
function PlugToLazy(plugin, opts)
    local lazySpec = {}
    if opts then
        lazySpec = opts
        lazySpec.ft = opts["for"]
        -- lazySpec.for = nil
        lazySpec.name = opts["as"]
        lazySpec.as = nil
        lazySpec.cmd = opts["on"]
        lazySpec.on = nil
        lazySpec.version = opts["tag"]
        if opts['afterLoad'] then
            lazySpec['config'] = function()
                if opts['afterLoad'] == true then
                    vim.fn[vim.fn.PluginNameToFunc(
                        vim.fn.GetPluginName(plugin)
                    )]()
                else
                    vim.fn[opts['afterLoad']]()
                end
            end
        end
        if lazySpec.cmd then
            if type(lazySpec.cmd) == "string" then
                lazySpec.cmd = {lazySpec.cmd}
            end
            -- <plug> mappings are commands ('on') for Plug, but keys for Lazy
            for k, cmd in pairs(lazySpec.cmd) do
                if string.find(string.lower(cmd), "<plug>", 1, 6) then
                    lazySpec.keys = lazySpec.keys or {}
                    -- Convert plug mappings for all modes, not just default of 'n'
                    table.insert(lazySpec.keys, {cmd, mode={'n','v','o','l'}})
                    lazySpec.cmd[k] = nil
                end
            end
            -- Remove empty cmd table to prevent lazyload
            if not lazySpec.cmd then lazySpec.cmd = nil end
        end
    end
    lazySpec[1] = plugin
    -- if string.find(plugin, "sandw") then print(vim.inspect(lazySpec)) end
    table.insert(MyLazySpecs, lazySpec)
end
EOF
endif

let s:PlugOpts = [
            \ 'branch',
            \ 'tag',
            \ 'commit',
            \ 'rtp',
            \ 'dir',
            \ 'as',
            \ 'do',
            \ 'on',
            \ 'for',
            \ 'frozen',
            \ ]

function! s:PluginAdapter(...)
    let l:plugin = a:1
    let l:plugin_name = GetPluginName(l:plugin)
    let l:args = {}
    if a:0 == 2
        let l:args = a:2
    endif
    if has('nvim')
        let g:__plugin_args= l:args
        exec 'lua PlugToLazy("' . l:plugin  . '", vim.g.__plugin_args)'
        let s:plugs[l:plugin_name] = 1
    else
        " convert args we want to keep
        for dep in get(l:args, 'dependencies', [])
            Plug dep
        endfor
        " Handle hook for after load
        let l:func = get(l:args, 'afterLoad', v:false)
        " If 'afterLoad' is v:true, call function based off a default name
        " convention (the plugin name, with _ replacing . and -). Otherwise
        " call the function name passed in. Only will be called for
        " lazy-loaded plugins, so don't use without an 'on' or 'for' mapping.
        " ('keys' gets ignored).
        if l:func == v:true
            exec 'au User ' . l:plugin_name . ' call ' . PluginNameToFunc(l:plugin_name) . '()'
        elseif l:func != v:false
            exec 'au User ' . l:plugin_name . ' call ' . l:func . '()'
        endif

        " Remove args unsupported by Plug
        for opt in keys(l:args)
            if index(s:PlugOpts, opt) < 0  " If item not in the list.
                silent! call remove(l:args, opt)
            endif
        endfor
        Plug l:plugin, l:args
    endif
endfunction

command! -bang -nargs=+ Plugin call <sid>PluginAdapter(<args>)

" Allow making a keys table with modes for Lazy.vim in vimscript
" usage: add plugin opt 'keys': MakeLazyKeys(["i%", "a%"], ["v","o"]),
lua << EOF
-- Returns a lua function for setting up a lazy keys spec. Need to return a
-- function because can't return a mixed list/dict table in vimscript.
MakeLazyKeys = function(keys, modes)
  return function()
      local ret = {}
      for _, key in ipairs(keys) do
        table.insert(ret, { key, mode = modes})
      end
      return ret
  end
end
EOF

function! MakeLazyKeys(keys, modes)
    return luaeval('MakeLazyKeys(_A[1], _A[2])', [a:keys, a:modes])
endf

function! GetPluginName(pluginUrl)
    return split(a:pluginUrl, '/')[-1]
endf

function! PluginNameToFunc(name)
    return 'Plug_after_' . substitute(a:name, '[\\.-]', '_', 'g')
endf
