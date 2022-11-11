require('refactoring').setup({})
local maps = vim.api.nvim_get_var('IDE_mappings')
-- prompt for a refactor to apply when the remap is triggered
-- if using telescope, we replace it with its mapping
if vim.fn.IsPluginUsed("telescope.nvim") == 0 then
   for _, mode in pairs({'v', 'n'}) do
      vim.api.nvim_set_keymap(
         mode,
         maps.refactor,
         ":lua require('refactoring').select_refactor()<CR>",
         { noremap = true, silent = true, expr = false }
         )
   end
end

-- generate annotations (eg docstrings)
require('neogen').setup {}
vim.cmd("command! Annotate lua require('neogen').generate()")

require('nvim-treesitter.configs').setup {
   -- ensure_installed = {
   --     "lua",
   --     "python",
   -- },
   endwise = { enable = true, },
   highlight = {
      enable = true,
      -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
      -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
      -- Using this option may slow down your editor, and you may see some duplicate highlights.
      -- Instead of true it can also be a list of languages
      -- additional_vim_regex_highlighting = false,
   },
}

require'nvim-treesitter.configs'.setup {
   context_commentstring = {
      enable = true
   }
}
require'treesitter-context'.setup{}
