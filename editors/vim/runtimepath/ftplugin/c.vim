autocmd BufNewFile *.\(cc\|hh\) 0put =\"//\<nl>// \".expand(\"<afile>:t\").\" -- \<nl>//\<nl>\"|2|start!
if has('nvim')
    " Stop #Defines being highilghted as comments by lsp/clangd
    lua vim.api.nvim_set_hl(0, '@lsp.type.comment.c', {})
endif
