
Plug 'autozimu/LanguageClient-neovim', {
            \ 'branch': 'next',
            \ 'do': 'bash install.sh',
            \ }
function! s:SetLSPShortcuts()
    nnoremap <leader>ld :call LanguageClient#textDocument_definition()<CR>
    nnoremap gd :call LanguageClient#textDocument_definition()<CR>
    nnoremap <leader>lt :call LanguageClient#textDocument_typeDefinition()<CR>
    nnoremap gy :call LanguageClient#textDocument_typeDefinition()<CR>
    nnoremap <leader>ln :call LanguageClient#textDocument_rename()<CR>
    nnoremap <leader>lf :call LanguageClient#textDocument_formatting()<CR>
    nnoremap <leader>lf :call LanguageClient#textDocument_references()<CR>
    nnoremap <leader>la :call LanguageClient_workspace_applyEdit()<CR>
    nnoremap <leader>lc :call LanguageClient#textDocument_completion()<CR>
    nnoremap <leader>lh :call LanguageClient#textDocument_hover()<CR>
    nnoremap <leader>ls :call LanguageClient_textDocument_documentSymbol()<CR>
    nnoremap <leader>lm :call LanguageClient_contextMenu()<CR>
endfunction()

let g:LanguageClient_diagnosticsDisplay = { 1: { "name": "Error",
            \ "signText": "X", },
            \ 2: { "name": "Warning",
            \ "signText": "!", },
            \ 3: { "name": "Information",
            \ "signText": "ℹ" },
            \ 4: { "name": "Hint",
            \ "signText": "?" }
            \ }

" let g:LanguageClient_loadSettings = 1 " Use an absolute configuration path if you want system-wide settings
" let g:LanguageClient_settingsPath = '/home/YOUR_USERNAME/.config/nvim/settings.json'
" let g:LanguageClient_serverCommands = {
" \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
" \ }
let g:LanguageClient_autoStart = 1
let g:LanguageClient_autoStop = 1
" Displays all messages at end of line.
" Can be a little annoying, so play with this as Error or Warning.
let g:LanguageClient_useVirtualText = 1
let g:LanguageClient_diagnosticsMaxSeverity = 'Warning'
" for documentation. Effective only when the floating window feature is
" supported.
let g:LanguageClient_useFloatingHover = 1
" Always preview with a hover rather than preview buffer.
" let g:LanguageClient_hoverPreview = 'never'

let g:LanguageClient_loggingFile = expand('~/.vim/LanguageClient.log')
autocmd myIDE FileType r,swift,cpp,c call <SID>SetLSPShortcuts()
" Install langserver with:
let b:cmdInstallRLSP ='if( !is.element("languageserver",
            \  .packages(all.available = TRUE))){
            \ install.packages("languageserver",repos="http://cran.us.r-project.org")}'
" Run and install with:
" R --slave -e 'b:cmdInstallRLSP; languageserver::run()'
let b:RLSPArray=['R', '--slave', '-e',
            \ b:cmdInstallRLSP . "; languageserver::run()"]

let g:LanguageClient_serverCommands = {
            \ '_': ['ale', ''],
            \ 'r': b:RLSPArray,
            \ 'swift': ['sourcekit-lsp'],
            \ }
