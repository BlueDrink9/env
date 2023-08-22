if vim.g.ideMode==0 then
  return {}
end

local maps = vim.api.nvim_get_var('IDE_mappings')

return {

  {'hrsh7th/cmp-nvim-lsp'},
  {'hrsh7th/cmp-buffer'},
  {'hrsh7th/cmp-path'},
  {'hrsh7th/nvim-cmp'},
  {'hrsh7th/cmp-vsnip'},
  {'hrsh7th/cmp-nvim-lsp-signature-help'},
  {'hrsh7th/cmp-nvim-lsp-document-symbol'},
  {'https://github.com/f3fora/cmp-spell'},

}
