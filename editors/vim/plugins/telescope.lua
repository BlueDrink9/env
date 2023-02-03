local maps = vim.api.nvim_get_var('IDE_mappings')
local telescope = require("telescope")
local telescope_mappings = {
   [maps.FuzzyFuzzy] = "builtin()",
   [maps.FuzzyOpenFile] = "find_files()",
   [maps.FuzzySearchFiles] = "live_grep()",
   [maps.FuzzySearchBuffers] = "live_grep({grep_open_files = true})",
   [maps.FuzzyBuffers] = "buffers()",
   [maps.FuzzyTags] = "tags()",
   [maps.FuzzyCommands] = "commands()",
}

function telescope_map(mappings, bufnr)
   for map, rhs in pairs(mappings) do
      local bind = ":lua require'telescope.builtin'."..rhs.."<CR>"
      local opts = { noremap = true, silent = true, expr = false }
      if bufnr then
         vim.api.nvim_buf_set_keymap( bufnr, 'n', map, bind, opts)
      else
         vim.api.nvim_set_keymap( 'n', map, bind, opts)
      end
   end
end

telescope_map(telescope_mappings)

-- By default, use treesitter. If an lsp client attaches that can replace it
-- however, use that instead.
if vim.fn.IsPluginUsed("nvim-treesitter") == 1 then
   telescope_map({treesitter = maps.FuzzySymbols})
end
vim.api.nvim_create_autocmd('LspAttach', {
   callback = function(args)
      local capabilities = vim.lsp.get_client_by_id(
         args.data.client_id).server_capabilites
      if not capabilities then return end
      if capabilities.workspaceSymbolProvider then
         telescope_map({lsp_workspace_symbols = maps.FuzzySymbols}, args.buf)
      elseif capabilities.documentSymbolProvider then
         telescope_map({lsp_document_symbols = maps.FuzzySymbols}, args.buf)
      end
   end
})

local opts = {
   defaults = {
      mappings = {
         i = {
            ["<C-h>"] = "which_key"
         },
      },
   },
   extensions = {},
   preview = False,
}


if vim.fn.IsPluginUsed("telescope-dap.nvim") == 1 then
   telescope.load_extension('dap')
end
if vim.fn.IsPluginUsed("telescope-vimspector.nvim") == 1 then
   telescope.load_extension("vimspector")
end
if vim.fn.IsPluginUsed("telescope-fzf-native.nvim") == 1 then
   telescope.load_extension("fzf")
end
if vim.fn.IsPluginUsed("telescope-coc.nvim") == 1 then
   opts.extensions.coc = {
         -- theme = 'ivy',
         prefer_locations = true, -- always use Telescope locations to preview definitions/declarations/implementations etc
      }
   telescope.load_extension('coc')
end
if vim.fn.IsPluginUsed('telescope-ui-select.nvim') == 1 then
   telescope.load_extension('ui-select')
   opts.extensions["ui-select"] = {
         require("telescope.themes").get_dropdown {
                 -- even more opts
         }
   }
end

if vim.fn.IsPluginUsed("refactoring.nvim") == 1 then
   telescope.load_extension('refactoring')
   for _, mode in pairs({'v', 'n'}) do
      vim.api.nvim_set_keymap(
         mode,
         maps.refactor,
         ":lua require('telescope').extensions.refactoring.refactors()<CR>",
         { noremap = true, silent = true, expr = false }
         )
   end
end

-- https://github.com/fcying/telescope-ctags-outline.nvim
opts.extensions.ctags_outline = {
   --ctags option
      ctags = {'ctags'},
   --ctags filetype option
      ft_opt = {
         vim = '--vim-kinds=fk',
         lua = '--lua-kinds=fk',
      },
}

-- Only rely on telescope (basically)
telescope.load_extension('changes')
telescope.load_extension("command_center")
telescope.load_extension('ctags_outline')
telescope.load_extension('repo')
telescope.load_extension('undo')

for map, rhs in pairs({
   [maps.FuzzyFuzzy.."u"] = "undo",
   [maps.FuzzyFuzzy.."r"] = "redo",
   [maps.FuzzyFuzzy.."c"] = "changes",
   [maps.FuzzyFuzzy.."m"] = "marks",
}) do
   local bind = "<cmd>Telescope "..rhs.."<cr>"
   local opts = { noremap = true, silent = true, expr = false }
   vim.keymap.set("n", map, bind, opts)
end

-- show current buf outline
-- telescope.extensions.ctags_outline.outline()
-- :Telescope ctags_outline outline

-- show all opened buf outline(use current buf filetype)
-- telescope.extensions.ctags_outline.outline({buf='all'})

telescope.setup(opts)
