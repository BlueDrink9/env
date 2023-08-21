if $USENF==1
  Plugin 'kyazdani42/nvim-web-devicons'
endif
Plugin 'romgrk/barbar.nvim'

" Move to previous/next
nnoremap <silent>    <Left> :BufferPrevious<CR>
nnoremap <silent>    <Right> :BufferNext<CR>
" Re-order to previous/next
nnoremap <silent>    <C-w <> :BufferMovePrevious<CR>
nnoremap <silent>    <C-w >> :BufferMoveNext<CR>
" Goto buffer in position...
nnoremap <silent>    <A-1> :BufferGoto 1<CR>
nnoremap <silent>    <A-2> :BufferGoto 2<CR>
nnoremap <silent>    <A-3> :BufferGoto 3<CR>
nnoremap <silent>    <A-4> :BufferGoto 4<CR>
nnoremap <silent>    <A-5> :BufferGoto 5<CR>
nnoremap <silent>    <A-6> :BufferGoto 6<CR>
nnoremap <silent>    <A-7> :BufferGoto 7<CR>
nnoremap <silent>    <A-8> :BufferGoto 8<CR>
nnoremap <silent>    <A-9> :BufferLast<CR>
" Close buffer
nnoremap <silent>    <A-c> :BufferClose<CR>
cabbrev! bd BufferClose
" Wipeout buffer
"                          :BufferWipeout<CR>
" Close commands
"                          :BufferCloseAllButCurrent<CR>
"                          :BufferCloseBuffersLeft<CR>
"                          :BufferCloseBuffersRight<CR>
" Magic buffer-picking mode
" nnoremap <silent> <C-s>    :BufferPick<CR>

" Other:
" :BarbarEnable - enables barbar (enabled by default)
" :BarbarDisable - very bad command, should never be used


" NOTE: If barbar's option dict isn't created yet, create it
let bufferline = get(g:, 'bufferline', {})
" Enable/disable auto-hiding the tab bar when there is a single buffer
let bufferline.auto_hide = v:true
"  - left-click: go to buffer
"  - middle-click: delete buffer
let bufferline.clickable = v:true

if $USENF==1
  let bufferline.icons = v:true
else
  let bufferline.icons = v:false
endif

" Configure icons on the bufferline.
let bufferline.icon_separator_active = '▎'
let bufferline.icon_separator_inactive = '▎'
let bufferline.icon_close_tab = ''
let bufferline.icon_close_tab_modified = '●'
let bufferline.maximum_padding = 4
let bufferline.maximum_length = 30
" If set, the letters for each buffer in buffer-pick mode will be
" assigned based on their name. Otherwise or in case all letters are
" already assigned, the behavior is to assign letters in order of
" usability (see order below)
let bufferline.semantic_letters = v:true
let bufferline.letters =
  \ 'arstneiodhkmxcvbzuywfplgjq;ARSTNEIODHKMXCVBZUYWFPLGJQ:'
