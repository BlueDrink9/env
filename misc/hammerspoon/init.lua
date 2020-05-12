function script_path()
   local str = debug.getinfo(2, "S").source:sub(2)
   return str:match("(.*/)")
end

dofile(script_path() .. "VimMode.spoon.lua")
