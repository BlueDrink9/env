vim.cmd("Plug 'https://github.com/neovim/nvim-lspconfig'")
-- Easy way to install lsps, that integrates well.
vim.cmd("Plug 'https://github.com/kabouzeid/nvim-lspinstall'")

vim.cmd("autocmd myPlugins User pluginSettingsToExec lua nvim_lspconfig_setup()")

function nvim_lspconfig_setup()
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
  -- Use a loop to conveniently call 'setup' on multiple servers and
  -- map buffer local keybindings when the language server attaches
  -- local servers = { "pyright", "rust_analyzer", "r_language_server", "diagnosticls", "dotls", "jedi_language_server", "texlab", "vimls" }
  local servers = require'lspinstall'.installed_servers()
  require'lspinstall'.setup()
  for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup {
      on_attach = on_attach,
      flags = {
        debounce_text_changes = 150,
      }
    }
  end
end
