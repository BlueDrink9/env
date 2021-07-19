-- Don't need to load this before any use of packer, only first time for
-- installing it. Packer compiles things so they will always be used.
-- Ensure packer is installed correctly.
local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
  execute 'packadd packer.nvim'
end

-- packer.reset()
-- packer.init(
--   {
--   }
--   )
