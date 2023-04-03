local M = {}

--- Usage: require'my.utils'.map_table_with_prefix({[maps.FuzzyOpenFile] = "find_files"}, "telescope.builtin")
function M.map_table_with_prefix(mappings, prefix, modes, opts)
   for keys, cmd in pairs(mappings) do
      vim.keymap.set(modes, keys, prefix..cmd.."<CR>", opts)
   end
end


function M.tostring(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. M.tostring(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end


return M
