-- vim: ft=lua
o = vim.opt
fn = vim.fn

-- set the cursor style when vim exits
-- vim.cmd('au VimLeave * set guicursor=a:block-blinkon0')  -- Uncomment if needed

-- Define VIMHOME based on platform
local is_windows = fn.has('win32') or fn.has('win64')
local VIMHOME = ""
if is_windows then
    VIMHOME = fn.expand("$HOME/vimfiles")
else
    VIMHOME = fn.expand("$HOME/.vim")
end

-- Update runtimepath
o.runtimepath:prepend(VIMHOME)
o.runtimepath:append(VIMHOME .. "/after")

-- Set packpath
o.packpath = o.runtimepath:get()

-- Uncomment if python providers are needed
-- vim.g.loaded_python_provider = 1
-- vim.g.python_host_skip_check = 1
-- vim.g.python_host_prog = fn.system('which python')
-- vim.g.python3_host_skip_check = 1
-- vim.g.python3_host_prog = fn.system('which python3')

-- Load the appropriate vimrc file based on platform and its existence
if is_windows then
    -- try load this vimrc
    if not pcall(vim.cmd, "source " .. fn.expand("~/vimfiles/vimrc")) then
        vim.cmd("source " .. fn.expand("~/_vimrc"))
    end
else
    if not pcall(vim.cmd, "source " .. fn.expand("~/.vim/vimrc")) then
        vim.cmd("source " .. fn.expand("~/.vimrc"))
    end
end
