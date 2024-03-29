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

function M.inject_dependency(specs, dependency_name)
   for _, spec in ipairs(specs) do
      if spec[1] ~= dependency_name and (
            spec.enabled == nil or spec.enabled
            ) then
         if spec.dependencies == nil then
            spec.dependencies = {}
         elseif type(spec.dependencies) == "string" then
            spec.dependencies = {spec.dependencies}
         end
         table.insert(spec.dependencies, dependency_name)
      end
   end
end

function M.fold_as_dependencies(specs, parent_spec_name)
   --- Given a list of specs and a parent spec name, adds all specs as dependencies of the
   --- parent spec (For lazy.nvim).
   local deps = {}
   for _, spec in ipairs(specs) do
      if spec[1] ~= parent_spec_name and (
            spec.enabled == nil or spec.enabled
            ) then
         table.insert(deps, spec[1])
      end
   end

   for _, spec in ipairs(specs) do
      if spec[1] ~= parent_spec_name then
         if spec.dependencies == nil then
            spec.dependencies = {}
         elseif type(spec.dependencies) == "string" then
            spec.dependencies = {spec.dependencies}
         end
         for _, dep in ipairs(deps) do
            table.insert(spec.dependencies, dep)
         end
      end
   end
end

return M
