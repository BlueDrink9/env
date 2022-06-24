vim.cmd("Plug 'nvim-lua/plenary.nvim'")
vim.cmd("Plug 'nvim-telescope/telescope.nvim'")
vim.cmd("autocmd myPlugins User pluginSettingsToExec lua telescope_setup()")

function telescope_setup()
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
end
