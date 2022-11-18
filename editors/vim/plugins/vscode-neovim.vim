" vim: foldmethod=marker
" vim: foldmarker={[},{]}

" {[} Mappings

" Not going to work because vscode-neovim doesn't do imaps
" inoremap kv :call VSCodeCall("vscode-neovim.escape")
" inoremap vk :call VSCodeCall("vscode-neovim.escape")

function VSCodeMaps()
let s:nmappings = {
      \ g:IDE_mappings.definition: 'editor.action.goToDeclaration',
      \ g:IDE_mappings.definition2: 'editor.action.peakDefinition',
      \ g:IDE_mappings.type_definition: 'editor.action.goToTypeDefinition',
      \ g:IDE_mappings.hover: 'editor.action.showHover',
      \ g:IDE_mappings.refactor: 'editor.action.refactor',
      \ g:IDE_mappings.rename: 'editor.action.rename',
      \ g:IDE_mappings.diagnostic: 'workbench.action.showErrorsWarnings',
      \ g:IDE_mappings.fix: 'editor.action.autoFix',
      \ g:IDE_mappings.codeAction: 'editor.action.quickFix',
      \ g:IDE_mappings.references: 'editor.action.findReferences',
      \ g:IDE_mappings.references2: 'editor.action.findReferences',
      \ g:IDE_mappings.FuzzyOpenFile: 'workbench.action.quickOpen',
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

for [key, value] in items(s:nmappings)
  exec 'nnoremap ' . key . " <Cmd>call VSCodeNotify('" . value . "')<CR>"
endfor
for [key, value] in items(s:vmappings)
  exec 'vnoremap ' . key . " <Cmd>call VSCodeNotifyVisual('" . value . "', 1)<CR>"
endfor


" {[} Window management
" leader w opens new vert window, switches to it
nnoremap <leader>w <C-w>v<C-w>l
" Edit current buffer with a new tab
nnoremap <C-w>t :tab sb<cr>
" Easier way to move between windows
nnoremap <C-h> <Cmd>call VSCodeNotify("workbench.action.focusLeftGroup")<CR>
nnoremap <C-l> <Cmd>call VSCodeNotify("workbench.action.focusRightGroup")<CR>
nnoremap <C-k> <Cmd>call VSCodeNotify("workbench.action.focusAboveGroup")<CR>
nnoremap <C-j> <Cmd>call VSCodeNotify("workbench.action.focusBelowGroup")<CR>

" {[} Open windows to the left, right, up, down, like in tmux
nnoremap <C-w>h <Cmd>call VSCodeNotify("workbench.action.splitEditorLeft")<CR>
nnoremap <C-w>l <Cmd>call VSCodeNotify("workbench.action.splitEditorRight")<CR>
nnoremap <C-w>k <Cmd>call VSCodeNotify("workbench.action.splitEditorUp")<CR>
nnoremap <C-w>j <Cmd>call VSCodeNotify("workbench.action.splitEditorDown")<CR>
" {]} Open windows to the left, right, up, down.

" Cycle through buffers
nnoremap <silent> <Right> :call VSCodeNotify("workbench.action.nextEditor")<CR>
nnoremap <silent> <Left> :call VSCodeNotify("workbench.action.previousEditor")<CR>
nnoremap <silent> <Up> :tabnext<CR>
nnoremap <silent> <Down> :tabprevious<CR>

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

" {]} Mappings

" Fix quickscope mappings (If used)
highlight QuickScopePrimary guifg='#afff5f' gui=underline ctermfg=155 cterm=underline
highlight QuickScopeSecondary guifg='#5fffff' gui=underline ctermfg=81 cterm=underline
