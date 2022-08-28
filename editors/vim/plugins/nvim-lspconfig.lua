if vim.g.plugs["nvim_lsp"] == nil then
   return
end
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
   local maps = vim.api.nvim_get_var('IDE_mappings')

   -- See `:help vim.lsp.*` for documentation on any of the below functions
   buf_set_keymap('n', maps.implementation, '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
   buf_set_keymap('n', maps.implementation2, '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
   buf_set_keymap('n', maps.definition, '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
   buf_set_keymap('n', maps.documentation, '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
   buf_set_keymap('n', maps.documentation2, '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
   buf_set_keymap('n', maps.documentation3, '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
   buf_set_keymap('n', maps.implementation, '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
   buf_set_keymap('n', maps.implementation2, '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
   buf_set_keymap('n', maps.signature, '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
   -- buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
   -- buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
   -- buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
   buf_set_keymap('n', maps.type_definition, '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
   buf_set_keymap('n', maps.type_definition2, '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
   buf_set_keymap('n', maps.rename, '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
   buf_set_keymap('n', maps.codeAction, '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
   buf_set_keymap('n', maps.references, '<cmd>lua vim.lsp.buf.references()<CR>', opts)
   buf_set_keymap('n', map.diagnostic, '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
   buf_set_keymap('n', maps.diagnostic_next, '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
   buf_set_keymap('n', maps.diagnostic_prev, '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
   -- buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
   buf_set_keymap("n", maps.reformat, "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

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
