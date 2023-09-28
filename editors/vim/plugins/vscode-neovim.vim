" vim: foldmethod=marker
" vim: foldmarker={[},{]}
" {[} Mappings

function! VSCodeMapDict(mappings, visualMappings)
    for [key, value] in items(a:mappings)
        exec 'nnoremap ' . key . " <Cmd>call VSCodeNotify('" . value . "')<CR>"
    endfor
    for [key, value] in items(a:visualMappings)
        " 0 to deselect visual after calling. Call rather than notify to
        " not deselect until after.
        exec 'vnoremap ' . key . " <Cmd>call VSCodeCall('" . value . "', 0)<CR>"
    endfor
endfunction

" Not going to work because vscode-neovim doesn't do imaps
" inoremap kv :call VSCodeCall("vscode-neovim.escape")
" inoremap vk :call VSCodeCall("vscode-neovim.escape")

function! VSCodeMaps()
let s:nmappings = {
      \ g:IDE_mappings.definition: 'editor.action.goToDeclaration',
      \ g:IDE_mappings.definition2: 'editor.action.peakDefinition',
      \ g:IDE_mappings.typeDefinition: 'editor.action.goToTypeDefinition',
      \ g:IDE_mappings.hover: 'editor.action.showHover',
      \ g:IDE_mappings.refactor: 'editor.action.refactor',
      \ g:IDE_mappings.rename: 'editor.action.rename',
      \ g:IDE_mappings.diagnostic: 'editor.action.showHover',
      \ g:IDE_mappings.fix: 'editor.action.autoFix',
      \ g:IDE_mappings.codeAction: 'editor.action.quickFix',
      \ g:IDE_mappings.references: 'editor.action.findReferences',
      \ g:IDE_mappings.references2: 'editor.action.findReferences',
      \ g:IDE_mappings.FuzzyOpenFile: 'workbench.action.quickOpen',
      \ g:IDE_mappings.FuzzyBuffers: 'workbench.action.quickOpen',
      \ g:IDE_mappings.FuzzyTags: 'workbench.action.showAllSymbols',
      \ g:IDE_mappings.FuzzyCommands: 'workbench.action.showCommands',
      \ g:IDE_mappings.FuzzyOldFiles: 'workbench.action.openRecent',
      \ g:IDE_mappings.GitCommit: 'git.commit',
      \ g:IDE_mappings.GitStage: 'git.stage',
      \ g:IDE_mappings.GitAmend: 'git.commitStagedAmend',
      \ g:IDE_mappings.debugFile: 'workbench.action.debug.run',
      \ '<c-s>': 'workbench.action.terminal.toggleTerminal',
      \ '<leader>a': 'workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup',
      \ }
      " \ g:IDE_mappings.definition: 'editor.action.revealDefinition',
      " \ g:IDE_mappings.type_definition: 'editor.action.revealDeclaration',
      " \ g:IDE_mappings.fix: 'editor.action.quickFix',

" 'tab' is just to sync the visual selection with vscode.
let s:vmappings = {
      \ '<tab>': '""',
      \ g:IDE_mappings.FuzzyCommands: 'workbench.action.showCommands',
      \ g:IDE_mappings.GitStage: 'git.stageSelectedRanges',
      \ g:IDE_mappings.GitUnstage: 'git.unstageSelectedRanges',
      \ }

call VSCodeMapDict(s:nmappings, s:vmappings)

" {[} Window management
" leader w opens new vert window, switches to it
nnoremap <leader>w <C-w>v<C-w>l
" Edit current buffer with a new tab
nnoremap <C-w>t :tab sb<cr>
" Easier way to move between windows
call VSCodeMapDict(
            \ {
            \ '<C-h>': 'workbench.action.focusLeftGroup',
            \ '<C-l>': 'workbench.action.focusRightGroup',
            \ '<C-k>': 'workbench.action.focusAboveGroup',
            \ '<C-j>': 'workbench.action.focusBelowGroup',
            \ },
            \ {
            \ },
            \)

" {[} Open windows to the left, right, up, down, like in tmux
call VSCodeMapDict(
            \ {
            \ '<C-w>h': 'workbench.action.splitEditorLeft',
            \ '<C-w>l': 'workbench.action.splitEditorRight',
            \ '<C-w>k': 'workbench.action.splitEditorUp',
            \ '<C-w>j': 'workbench.action.splitEditorDown',
            \ },
            \ {
            \ },
            \)

" {]} Open windows to the left, right, up, down.

" Cycle through buffers
call VSCodeMapDict(
            \ {
            \ '<Right>': 'workbench.action.nextEditor',
            \ '<Left>': 'workbench.action.previousEditor',
            \ },
            \ {
            \ },
            \)
" nnoremap <silent> <Up> :tabnext<CR>
" nnoremap <silent> <Down> :tabprevious<CR>

" Easy resize
nnoremap <S-Right> 5<C-W>>
nnoremap <S-Left> 5<C-W><
nnoremap <S-Up> 3<C-W>+
nnoremap <S-Down> 3<C-W>-
" Zoom a window into its own tab.
noremap <silent> <C-w>z :tab split<CR>
" Kill current buffer. Complete bdel because may use Bdelete, not bdelete.
noremap <silent> <C-w>x :bdel<tab><CR>
" if has("gui")
"     " If window id of last window is 1, assume only one window present
"     if winnr($) == 1

" {]} Window management

xmap <leader>c  <Plug>VSCodeCommentary
nmap <leader>c  <Plug>VSCodeCommentary
omap <leader>c  <Plug>VSCodeCommentary
nmap <leader>cc <Plug>VSCodeCommentaryLine

" Undo my \v mapping
nnoremap / /

nnoremap n n
nnoremap N N

endfunction
au myVimrc User MappingOverrides call VSCodeMaps()


" FT specific
function! VSCodeFTMaps(ft)
      if a:ft ==? 'sql'
            let b:REPLSendCommand = 'mssql.runQuery'
            call VSCodeMapDict(
                              \ {
                              \ g:IDE_mappings.REPLSendFile: b:REPLSendCommand,
                              \ g:IDE_mappings.REPLCancel: 'mssql.cancelQuery',
                              \},
                              \ {
                              \ g:IDE_mappings.REPLSend: 'mssql.runQuery',
                              \},
                              \)
      elseif a:ft ==? 'python'
            let b:REPLSendCommand = 'jupyter.execSelectionInteractive'
            call VSCodeMapDict(
                              \ {
                              \ g:IDE_mappings.REPLSendFile: 'jupyter.runFileInteractive',
                              \},
                              \ {
                              \ g:IDE_mappings.REPLSend: b:REPLSendCommand,
                              \},
                              \)
      endif
      function! _OpfuncRunMotion(type = '') abort
            if a:type == ''
                  set opfunc=_OpfuncRunMotion
                  return 'g@'
            endif
            if !exists('b:REPLSendCommand')
                  echom "no repl command set for this filetype: '" . &ft . "'"
                  return
            endif
            let [line1, line2] = [line("'["), line("']")]
            " 0 to deselect visual after calling. Call rather than notify to
            " not deselect until after.
            call VSCodeCallRange(b:REPLSendCommand, line1, line2, 0)
      endfunction
      exec 'nnoremap <expr> ' . g:IDE_mappings.REPLSend .
                        \ ' _OpfuncRunMotion()'
      exec 'nnoremap <expr> ' . g:IDE_mappings.REPLSendLine .
                        \ " _OpfuncRunMotion() .. '_'"
endfunction

au myPlugins filetype sql call VSCodeFTMaps('sql')
au myPlugins filetype python call VSCodeFTMaps('python')

" {]} Mappings
