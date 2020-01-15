function s:SID()
  return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfun
if has("nvim")
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
    call add(g:pluginSettingsToExec,
                \ "call deoplete#custom#option('yarp', v:true)")
endif
Plug 'Shougo/denite.nvim'
" deoplete tab-complete - Supertab handles this.
let g:deoplete#enable_at_startup = 1

" let g:deoplete#enable_smart_case = 1
" Num processes = 0 means number of sources.
call add(g:pluginSettingsToExec, "call deoplete#custom#option(
            \ 'smart_case', v:true,
            \ 'num_processes', 0,
            \ )")

" Completion sources {[}
" complete from syntax files
Plug 'Shougo/neco-syntax'
Plug 'Shougo/neoinclude.vim'
" Completion sources for vimscript
Plug 'Shougo/neco-vim'
Plug 'artur-shaik/vim-javacomplete2'

call add(g:pluginSettingsToExec,
            \ "call deoplete#custom#source('ultisnips', 'matchers', ['matcher_fuzzy'])")
call add(g:pluginSettingsToExec,
            \ "call deoplete#custom#source('LanguageClient', 'min_pattern_length', 2)")
" autocmd FileType x
" \ call deoplete#custom#buffer_option('auto_complete', v:false)

" Plug 'https://github.com/lionawurscht/deoplete-biblatex'
Plug 'deoplete-plugins/deoplete-tag'
call add(g:pluginSettingsToExec, "call deoplete#custom#option('omni_patterns', {
            \ 'r': ['[^. *\t]\.\w*', '\h\w*::\w*', '\h\w*\$\w*'],
            \})")
            " \ 'r': '[^. *\t]\.\w*',

call add(g:pluginSettingsToExec, "
            \ call deoplete#custom#var('omni', 'input_patterns', {
            \ 'tex': g:vimtex#re#deoplete,
            \})
            \ ")
" This was in the previous dict.
" See https://github.com/Shougo/deoplete.nvim/issues/745
            " \ 'r': '[^. *\t]\.\w*'
call add(g:pluginSettingsToExec,
            \ "call deoplete#custom#source('dictionary', 'min_pattern_length', 4)")
" Rank 200 is below around, above file, below omni.
" Note: omni not async :(
call add(g:pluginSettingsToExec,
            \ "call deoplete#custom#source('buffer', 'rank', 200)")
if executable("clang")
    Plug 'Shougo/deoplete-clangx', {'for': ['c', 'cpp'] }
endif
Plug 'deoplete-plugins/deoplete-dictionary'
" exec "Plug 'deoplete-plugins/deoplete-dictionary',
"             \ { 'for': " . g:proseFileTypes . " }"

" Completion sources {]}

" deoplete usually only completes from other buffers with the same
" filetype. This is a way of adding additional fts to complete from.
" Plug 'Shougo/context_filetype.vim'
if !exists('g:context_filetype#same_filetypes')
  let g:context_filetype#same_filetypes = {}
endif
" In tex buffers, completes from .bib.
let g:context_filetype#same_filetypes.tex = 'bib'
" In gitconfig buffers, completes from all buffers.
let g:context_filetype#same_filetypes.gitconfig = '_'
" " In default, completes from all buffers.
" let g:context_filetype#same_filetypes._ = '_'
" Todo: Fix context and LanguageClient interactions. For some reason
" don't work properly together, and completion from other buffers with
" different filetypes fails.
call add(g:pluginSettingsToExec, "
\ call deoplete#custom#var('buffer', 'require_same_filetype', v:false)
            \")

" {[} ---------- Denite ----------
" From https://github.com/ctaylo21/jarvis/blob/master/config/nvim/init.vim#L58
" Wrap in try/catch to avoid errors on initial install before plugin is available
" try
" catch
"     echo 'Denite not installed. It should work after running :PlugInstall'
" endtry
function! s:deniteSetup()
    " === Denite setup ==="
    " Use ripgrep for searching current directory for files
    " By default, ripgrep will respect rules in .gitignore
    "   --files: Print each file that would be searched (but don't search)
    "   --glob:  Include or exclues files for searching that match the given glob
    "            (aka ignore .git files)
    "
    call denite#custom#var('file/rec', 'command', ['rg', '--files', '--glob', '!.git'])

    " Use ripgrep in place of "grep"
    call denite#custom#var('grep', 'command', ['rg'])

    " Custom options for ripgrep
    "   --vimgrep:  Show results with every match on it's own line
    "   --hidden:   Search hidden directories and files
    "   --heading:  Show the file name above clusters of matches from each file
    "   --S:        Search case insensitively if the pattern is all lowercase
    call denite#custom#var('grep', 'default_opts', ['--hidden', '--vimgrep', '--heading', '-S'])

    " Recommended defaults for ripgrep via Denite docs
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])

    " Remove date from buffer list
    call denite#custom#var('buffer', 'date_format', '')

    " Open file commands
    call denite#custom#map('normal', "<leader>f", '<denite:do_action:split>')
    " call denite#custom#map('insert,normal', "<C-t>", '<denite:do_action:tabopen>')
    " call denite#custom#map('insert,normal', "<C-v>", '<denite:do_action:vsplit>')
    " call denite#custom#map('insert,normal', "<C-h>", '<denite:do_action:split>')

    " Custom options for Denite
    "   auto_resize             - Auto resize the Denite window height automatically.
    "   prompt                  - Customize denite prompt
    "   direction               - Specify Denite window direction as directly below current pane
    "   winminheight            - Specify min height for Denite window
    "   highlight_mode_insert   - Specify h1-CursorLine in insert mode
    "   prompt_highlight        - Specify color of prompt
    "   highlight_matched_char  - Matched characters highlight
    "   highlight_matched_range - matched range highlight
    let s:denite_options = {'default' : {
                \ 'auto_resize': 1,
                \ 'prompt': 'λ:',
                \ 'direction': 'rightbelow',
                \ 'winminheight': '10',
                \ 'highlight_mode_insert': 'Visual',
                \ 'highlight_mode_normal': 'Visual',
                \ 'prompt_highlight': 'Function',
                \ 'highlight_matched_char': 'Function',
                \ 'highlight_matched_range': 'Normal'
                \ }}

    " Loop through denite options and enable them
    function! s:profile(opts) abort
        for l:fname in keys(a:opts)
            for l:dopt in keys(a:opts[l:fname])
                call denite#custom#option(l:fname, l:dopt, a:opts[l:fname][l:dopt])
            endfor
        endfor
    endfunction


    call s:profile(s:denite_options)
endfunction
let s:deniteSetupCall = GetLocalFunctionCall(s:SID(), 'deniteSetup()')
call add(g:pluginSettingsToExec, s:deniteSetupCall)
" {]} ---------- Denite ----------
