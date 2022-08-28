local maps = vim.api.nvim_get_var('IDE_mappings')
telescope_mappings = {
   find_files = maps.FuzzyOpenFile,
   live_grep = maps.FuzzySearchFiles,
   buffers = maps.FuzzySearchBuffers,
   tags = maps.FuzzyTags,
   commands = maps.FuzzyCommands,
   lsp_workspace_symbols = maps.FuzzyLspTags,
}

for f, map in pairs(telescope_mappings) do
   vim.api.nvim_set_keymap(
      'n',
      map,
      ":lua require'telescope.builtin'."..f.."{}<CR>",
      { noremap = true, silent = true, expr = false }
      )
end

require('telescope').setup{
   defaults = {
      mappings = {
         i = {
            ["<C-h>"] = "which_key"
         },
      },
   },
}

require('telescope').load_extension('changes')

require("telescope").load_extension("command_center")

if vim.fn.IsPluginUsed("telescope-dap.nvim") then
   require('telescope').load_extension('dap')
end
if vim.fn.IsPluginUsed("telescope-vimspector.nvim") then
   require("telescope").load_extension("vimspector")
end
if vim.fn.IsPluginUsed("telescope-coc.nvim") then
   require("telescope").setup({
         extensions = {
            coc = {
               theme = 'ivy',
               prefer_locations = true, -- always use Telescope locations to preview definitions/declarations/implementations etc
            }
         },
      })
   require('telescope').load_extension('coc')
end

-- https://github.com/fcying/telescope-ctags-outline.nvim
require('telescope').setup{
   extensions = {
      ctags_outline = {
         --ctags option
         ctags = {'ctags'},
         --ctags filetype option
         ft_opt = {
            vim = '--vim-kinds=fk',
            lua = '--lua-kinds=fk',
         },
      },
   },
}

require('telescope').load_extension('ctags_outline')
-- show current buf outline
-- require('telescope').extensions.ctags_outline.outline()
-- :Telescope ctags_outline outline

-- show all opened buf outline(use current buf filetype)
-- require('telescope').extensions.ctags_outline.outline({buf='all'})
