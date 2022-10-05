if vim.g.plugs["nvim_lsp"] == nil then
   return
end

local maps = vim.api.nvim_get_var('IDE_mappings')
-- Mappings.
-- See `:help vim.lsp.*` for documentation on any of the below functions
local lsp_nbufmaps = {
   ['<space>wa'] = 'buf.add_workspace_folder()',
   [maps.implementation] = 'buf.declaration()',
   [maps.implementation2] = 'buf.declaration()',
   [maps.definition] = 'buf.definition()',
   [maps.documentation] = 'buf.hover()',
   [maps.documentation2] = 'buf.hover()',
   [maps.documentation3] = 'buf.hover()',
   [maps.implementation] = 'buf.implementation()',
   [maps.implementation2] = 'buf.implementation()',
   [maps.type_definition] = 'buf.type_definition()',
   [maps.type_definition2] = 'buf.type_definition()',
   [maps.rename] = 'buf.rename()',
   [maps.codeAction] = 'buf.code_action()',
   [maps.references] = 'buf.references()',
   [map.diagnostic] = 'diagnostic.show_line_diagnostics()',
   [maps.diagnostic_next] = 'diagnostic.goto_prev()',
   [maps.diagnostic_prev] = 'diagnostic.goto_next()',
   [maps.reformat] = 'buf.formatting()',
--  ['<space>wr'] = 'buf.remove_workspace_folder()',
--  ['<space>wl'] = '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))',
--  ['<space>q'] = 'diagnostic.set_loclist()',
}

nvim_lsp = require('lspconfig')
nvim_lspconfig_server_setup(nvim_lsp)
-- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
require'lspinstall'.post_install_hook = function ()
   nvim_lspconfig_server_setup() -- reload installed servers
   vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
end

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
   local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
   local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

   --Enable completion triggered by <c-x><c-o>
   buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

   -- Mappings.
   local opts = { noremap=true, silent=true }
   for key, cmd in pairs(to_bufmap) do
      buf_set_keymap('n', key, "<cmd>lua vim.lsp." .. cmd .. "()<CR>", opts)
   end

end

nvim_lspconfig_server_setup = function(nvim_lsp)
   local lsp_installer = require("nvim-lsp-installer")
   lsp_installer.settings({
         ui = {
            icons = {
               server_installed = "✓",
               server_pending = "➜",
               server_uninstalled = "✗"
            }
         }
      })
   -- Register a handler that will be called for each installed server when it's ready (i.e. when installation is finished
   -- or if the server is already installed).
   lsp_installer.on_server_ready(function(server)
      local opts = {}

      -- (optional) Customize the options passed to the server
      -- if server.name == "tsserver" then
      --     opts.root_dir = function() ... end
      -- end

      -- This setup() function will take the provided server configuration and decorate it with the necessary properties
      -- before passing it onwards to lspconfig.
      -- Refer to https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
      server:setup(opts)
   end)
end
