local maps = vim.api.nvim_get_var('IDE_mappings')
local telescope = require("telescope")
vim.api.nvim_set_keymap(
   'n',
   maps.FuzzySearchBuffers,
   ":lua require'telescope.builtin'.live_grep({grep_open_files = true})<CR>",
   { noremap = true, silent = true, expr = false }
   )
local telescope_mappings = {
   find_files = maps.FuzzyOpenFile,
   live_grep = maps.FuzzySearchFiles,
   buffers = maps.FuzzyBuffers,
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


telescope.setup{
   defaults = {
      mappings = {
         i = {
            ["<C-h>"] = "which_key"
         },
      },
   },
}


if vim.fn.IsPluginUsed("telescope-dap.nvim") == 1 then
   telescope.load_extension('dap')
end
if vim.fn.IsPluginUsed("telescope-vimspector.nvim") == 1 then
   telescope.load_extension("vimspector")
end
if vim.fn.IsPluginUsed("telescope-fzf-native.nvim") == 1 then
   telescope.load_extension("fzf")
end
if vim.fn.IsPluginUsed("telescope-coc.nvim") == 1 then
   telescope.setup({
         extensions = {
            coc = {
               -- theme = 'ivy',
               prefer_locations = true, -- always use Telescope locations to preview definitions/declarations/implementations etc
            }
         },
      })
   telescope.load_extension('coc')
end

if vim.fn.IsPluginUsed("refactoring.nvim") == 1 then
   telescope.load_extension('refactoring')
   for _, mode in pairs({'v', 'n'}) do
      vim.api.nvim_set_keymap(
         mode,
         maps.refactor,
         ":lua require('telescope').extensions.refactoring.refactors()<CR>",
         { noremap = true, silent = true, expr = false }
         )
   end
end

-- https://github.com/fcying/telescope-ctags-outline.nvim
telescope.setup{
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

if vim.fn.IsPluginUsed("telescope-changes.nvim") == 1 then
-- if vim.fn.IsPluginUsed("command_center.nvim") == 1 then
   telescope.load_extension('changes')
   telescope.load_extension("command_center")
   telescope.load_extension('ctags_outline')
end

-- show current buf outline
-- telescope.extensions.ctags_outline.outline()
-- :Telescope ctags_outline outline

-- show all opened buf outline(use current buf filetype)
-- telescope.extensions.ctags_outline.outline({buf='all'})
