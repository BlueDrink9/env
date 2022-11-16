-- This config is designed to set up and use LSPs automatically, so long as the
-- required server is installed. If the server is installed, no additional
-- config is required unless modifying behaviour specific to that server.
-- Additionally, installation is made trivial by Mason's :LspInstall, which
-- installs a relevant server for the current filetype.

local maps = vim.g.IDE_mappings
-- Mappings.
-- See `:help vim.lsp.*` for documentation on any of the below functions
local lsp_nbufmaps = {
   [maps.implementation] = 'buf.declaration()',
   [maps.implementation2] = 'buf.declaration()',
   [maps.definition] = 'buf.definition()',
   [maps.definition2] = 'buf.definition()',
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
   [maps.references2] = 'buf.references()',
   [maps.reformat] = 'buf.formatting()',
   -- ['<space>wa'] = 'buf.add_workspace_folder()',
   -- ['<space>wr'] = 'buf.remove_workspace_folder()',
   --  ['<space>wl'] = '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders))()',
}

local diagnostic_nbufmaps = {
  [maps.listErrs] = 'set_loclist()',
  [maps.diagnostic] = 'open_float(nil, {focus=false})',
  [maps.diagnostic_prev] = 'goto_prev()',
  [maps.diagnostic_next] = 'goto_next()',
}

local lsp_flags = {
   debounce_text_changes = 200,
}

require('lspconfig')
require("mason").setup()


if vim.g.plugs["nvim-cmp"] ~= nil then
  Nvim_cmp_capabilities = require('cmp_nvim_lsp').default_capabilities()

  Nvim_cmp_capabilities.textDocument.completion.completionItem.snippetSupport = true
  Nvim_cmp_capabilities.textDocument.completion.completionItem.resolveSupport = {
    properties = {
      'documentation',
      'detail',
      'additionalTextEdits',
    }
  }
end

-- Use an on_attach function to only map keys etc after the language server
-- attaches to the current buffer
local on_attach = function(client, bufnr)
   local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
   local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
   vim.api.nvim_create_augroup("lsp_on_attach", { clear = true })

   --Enable completion triggered by <c-x><c-o>
   buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

   -- Auto-show diagnostics on pause over them.
   vim.api.nvim_create_autocmd(
    { "CursorHold", "CursorHoldI"},
    {
     group = "lsp_on_attach",
     buffer = bufnr,
     callback = function() vim.diagnostic.open_float(nil, {focus=false}) end,
    })
   vim.api.nvim_create_autocmd(
    { "CursorHold", "CursorHoldI"},
    {
     group = "lsp_on_attach",
     buffer = bufnr,
     callback = require('nvim-lightbulb').update_lightbulb,
    })


  -- Mappings.
  local prefixes = {lsp="vim.lsp.", diag="vim.diagnostic."}
  local to_map_tables = {lsp=lsp_nbufmaps, diag=diagnostic_nbufmaps}
  for k, table in pairs(to_map_tables) do
    for key, cmd in pairs(table) do
      local rhs = "<cmd>lua " .. prefixes[k] .. cmd .. "<CR>"
      buf_set_keymap('n', key, rhs,
        { noremap=true, silent=true })
    end
  end
end

-- require("nvim.diagnostic_virtual_text_config").setup {}
vim.diagnostic.config({
    virtual_text = false,
  })

local lsp_installer = require("mason-lspconfig")

lsp_installer.setup({
      -- ensure_installed = { "pylsp" },
   })


local default_handler = function (server_name)
   require("lspconfig")[server_name].setup {
      on_attach = on_attach,
      capabilities = Nvim_cmp_capabilities,
      flags = lsp_flags,
   }
end

-- Register a handler that will be called for each installed server when it's
-- ready (i.e. when installation is finished or if the server is already
-- installed).
lsp_installer.setup_handlers({
      default_handler,

   -- (optional) Customize the options passed to the server
   ["pylsp"] = function ()
      default_handler("pylsp")
      -- vim.cmd("UnPlug 'davidhalter/jedi-vim'")
      vim.cmd [[let g:jedi#auto_initialization = 0]]
      vim.cmd [[let g:pymode = 0]]
      vim.cmd [[silent! au! myPymode"]]
   end,

   ["sumneko_lua"] = function ()
      local server_name = "sumneko_lua"
      require("lspconfig")[server_name].setup {
         on_attach = on_attach,
         capabilities = Nvim_cmp_capabilities,
         settings = {
            -- Get the language server to recognize the `vim` global
            Lua = { diagnostics = { globals = {'vim'}, }, },
         },
      }
   end,
   ["sqls"] = function()
      require('lspconfig').sqls.setup{
         on_attach = function(client, bufnr)
            require('sqls').on_attach(client, bufnr)
            on_attach(client, bufnr)
            vim.api.nvim_buf_set_keymap(
               bufnr, '', maps.REPLSend, "<Plug>(sqls-execute-query)",
               { noremap=true, silent=true })
            vim.api.nvim_buf_set_keymap(
               bufnr, 'n', maps.REPLSendLine, "0<Plug>(sqls-execute-query)$",
               { noremap=true, silent=true })
         end
      }
   end
})

-- Instead of showing signs, change the colour of the numbercolumn.
-- vim.fn.sign_define("LspDiagnosticsSignError", {text = "", numhl = "LspDiagnosticsDefaultError"})
-- vim.fn.sign_define("LspDiagnosticsSignWarning", {text = "", numhl = "LspDiagnosticsDefaultWarning"})
-- vim.fn.sign_define("LspDiagnosticsSignInformation", {text = "", numhl = "LspDiagnosticsDefaultInformation"})
-- vim.fn.sign_define("LspDiagnosticsSignHint", {text = "", numhl = "LspDiagnosticsDefaultHint"})

require("trouble").setup {
   action_keys = { -- key mappings for actions in the trouble list
      toggle_fold = {"zA", "za", "<BS>"},
   },
   auto_close = true,
   use_diagnostic_signs = true, -- enabling this will use the signs defined in your lsp client
   icons = function()
      if os.getenv("USENF") == 1 then
         return true
      else
         return false
      end
   end,
}
