require('refactoring').setup({})
local maps = vim.api.nvim_get_var('IDE_mappings')
-- prompt for a refactor to apply when the remap is triggered
for _, mode in pairs({'v', 'n'}) do
    vim.api.nvim_set_keymap(
    mode,
    maps.refactor,
    ":lua require('refactoring').select_refactor()<CR>",
    { noremap = true, silent = true, expr = false }
    )
end

-- generate annotations (eg docstrings)
require('neogen').setup {}
vim.cmd("command! Annotate lua require('neogen').generate()")

require('nvim-treesitter.configs').setup { endwise = { enable = true, }, }
