local M = {}

--- Usage: require'my.utils'.map_table_with_prefix({[maps.FuzzyOpenFile] = "find_files"}, "telescope.builtin")
function M.map_table_with_prefix(mappings, prefix, modes, opts)
   for keys, cmd in pairs(mappings) do
      vim.keymap.set(modes, keys, prefix..cmd.."<CR>", opts)
   end
end

return M
