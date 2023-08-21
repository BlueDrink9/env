
" On windows, the binary is an exe, so windows and WSL plugins can share
" a folder and have different binary names.
command! LanguageClientUpdateAndInstallBinary PlugInstall! LanguageClient-neovim

function! s:SetLSPShortcuts()
    let g:mappings_dict = {
    \ g:IDE_mappings.definition: "textDocument_definition",
    \ g:IDE_mappings.definition2: "textDocument_definition",
    \ g:IDE_mappings.typeDefinition: "textDocument_typeDefinition",
    \ g:IDE_mappings.rename: "textDocument_rename",
    \ g:IDE_mappings.reformat: "textDocument_formatting",
    \ g:IDE_mappings.references: "textDocument_references",
    \ g:IDE_mappings.references2: "textDocument_references",

    \ g:IDE_mappings.codeAction: "workspace_applyEdit",
    \ g:IDE_mappings.fix: "workspace_applyEdit",
    \ g:IDE_mappings.complete: "textDocument_completion",
    \ g:IDE_mappings.codelensAction: "textDocument_hover",
    \ g:IDE_mappings.documentation: "textDocument_documentSymbol",
    \ g:IDE_mappings.documentation2: "textDocument_documentSymbol",
    \ g:IDE_mappings.documentation3: "textDocument_documentSymbol",
    \ g:IDE_mappings.allActions: "contextMenu",
    \ }

    for [lhs, rhs] in items(g:mappings_dict)
        exec 'nnoremap :call LanguageClient#' . lhs . '()<CR> ' . rhs
    endfor
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
" Check for successful loading. Failure means binary not available, in which
" case we don't want to clobber any mappings.
autocmd myIDE FileType * if exists('g:LanguageClient_loaded') && g:LanguageClient_loaded == 1
            \ | call <SID>SetLSPShortcuts() | endif
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
