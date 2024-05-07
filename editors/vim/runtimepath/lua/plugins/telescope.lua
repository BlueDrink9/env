if vim.g.ideMode==0 then
  return {}
end

local telecope_make_cmd = ""
if vim.fn.Executable('cmake') == 1 then
  telecope_make_cmd = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build'
elseif vim.fn.Executable('make') == 1 then
  telecope_make_cmd = "make"
else
  telecope_make_cmd = ""
end

local maps = vim.api.nvim_get_var('IDE_mappings')

local build_keymaps_other = function(key, ext)
  return {maps.FuzzyFuzzy..key, "<cmd>Telescope " .. ext .. "<cr>"}
end


local specs = {
  {'junegunn/fzf.vim', enabled=false},
  {'junegunn/fzf', enabled=false},

  {
    'nvim-telescope/telescope.nvim',
    version='*',
    -- {'my.utils'}},
    cmd = "Telescope",
    keys = function()
      local out = {}
      table.insert(out, build_keymaps_other("m", "marks"))
      for keys, cmd in pairs({
        [maps.FuzzyFuzzy]         = "builtin()",
        [maps.FuzzyOpenFile]      = "find_files()",
        [maps.FuzzySearchFiles]   = "live_grep()",
        [maps.FuzzySearchBuffer]  = "current_buffer_fuzzy_find()",
        [maps.FuzzySearchBuffers] = "live_grep({grep_open_files = true})",
        [maps.FuzzyBuffers]       = "buffers()",
        [maps.FuzzyTags]          = "tags()",
        [maps.FuzzyCommands]      = "commands()",
        ["<leader>:"]             = "command_history()",
        [maps.FuzzyOldFiles]      = "oldfiles()",
        -- By default, use treesitter for symbols. If an lsp client
        -- attaches which can replace it however, replace with that.
        [maps.FuzzySymbols]       = "treesitter()",
      }) do
        local map_prefix = "require'telescope.builtin'."
        cmdStr = map_prefix..cmd
        table.insert(out, {keys, loadstring(cmdStr), desc=cmdStr})
      end

      -- for keys, cmd in pairs({
      --   -- extra manual/insert ones
      -- }) do
      --   local map_prefix = "require'telescope'."
      --   table.insert(out, {keys, loadstring(map_prefix..cmd), mode='i'})
      -- end

      return out
    end,

    opts = function(_, opts)

      local map_prefix = "<cmd>lua require'telescope.builtin'."
      local actions = require("telescope.actions")

      -- Mapping this manually because the lazy.nvim one seems to fail
      -- with keys with different prefixes.
      vim.keymap.set("n", maps.FuzzyOldFiles, map_prefix .. "oldfiles()<cr>", {desc="oldfiles"})

      vim.api.nvim_create_autocmd('LspAttach', {
        callback = function(args)
          -- For some reason capabilities is always nil at this point. The
          -- equivalent in lspconfig works though.
          local capabilities = vim.lsp.get_client_by_id(
            args.data.client_id).server_capabilites
          if not capabilities then return end
          if capabilities.workspaceSymbolProvider then
            require'my.utils'.map_table_with_prefix(
              {[maps.FuzzySymbols] = "lsp_document_symbols"},
              map_prefix, 'n', {buffer=args.buf})
          end
          if capabilities.documentSymbolProvider then
            require'my.utils'.map_table_with_prefix(
              {[maps.FuzzySymbolsWorkspace] = "lsp_workspace_symbols"},
              map_prefix, 'n', {buffer=args.buf})
          end
        end
      })

      opts = vim.tbl_deep_extend("force", opts, {
        defaults = {
          mappings = {
            i = {
              ["<C-h>"] = "which_key",
              ["<C-Down>"] = actions.cycle_history_next,
              ["<C-Up>"] = actions.cycle_history_prev,
              ["<M-Down>"] = actions.cycle_history_next,
              ["<M-Up>"] = actions.cycle_history_prev,
              ["<esc>"] = actions.close

            },
          },
        },
        pickers = {
          colorscheme = {
            enable_preview = true,
          },
        },
        extensions = {},
      })
      if vim.fn.has('win32') == 1 then
        -- Super slow on windows for some reason
        opts.defaults.preview = false
      end

      vim.api.nvim_create_user_command('Colorschemes', 'Telescope colorscheme', {})

      opts.extensions.coc = {
        -- theme = 'ivy',
        prefer_locations = true, -- always use Telescope locations to preview definitions/declarations/implementations etc
      }
      opts.extensions["ui-select"] = {
        require("telescope.themes").get_dropdown {
          -- even more opts
        }
      }

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

      -- show current buf outline
      -- telescope.extensions.ctags_outline.outline()
      -- :Telescope ctags_outline outline

      -- show all opened buf outline(use current buf filetype)
      -- telescope.extensions.ctags_outline.outline({buf='all'})
      return opts
    end,

    dependencies = {

      {'nvim-lua/plenary.nvim'},

      -- All installed with telescope IDE plugins
      -- require("telescope") only, basics

      -- config = function() require("telescope").load_extension("vimspector") end,
      -- config = function() require("telescope").load_extension('coc') end,
      -- config = function() require("telescope").load_extension('refactoring') end,

      -- require('my.utils').map_table_with_prefix({
      --   [maps.FuzzyFuzzy.."m"] = "marks",
      -- }, "<cmd>Telescope ", "n")

      {'https://github.com/nvim-telescope/telescope-ui-select.nvim',
        config = function() require("telescope").load_extension('ui-select') end,
      },

      {'LinArcX/telescope-changes.nvim',
        config = function() require("telescope").load_extension('changes') end,
        keys = build_keymaps_other("c", "changes"),
      },

      {'FeiyouG/command_center.nvim',
        config = function() require("telescope").load_extension("command_center") end,
      },

      {'https://github.com/debugloop/telescope-undo.nvim',
        config = function() require("telescope").load_extension('undo') end,
        keys = build_keymaps_other("u", "undo"),
      },

      {'cljoly/telescope-repo.nvim',
        config = function() require("telescope").load_extension('repo') end,
        keys = build_keymaps_other("r", "redo"),
      },

      {'fcying/telescope-ctags-outline.nvim',
        config = function() require("telescope").load_extension('ctags_outline') end,
      },

      {
        'nvim-telescope/telescope-fzf-native.nvim',
        config = function() require("telescope").load_extension("fzf") end,
        enabled = vim.fn.IsCCompilerAvailable() and
          (vim.fn.Executable('cmake') == 1 or vim.fn.Executable('make') == 1),
        build = telecope_make_cmd,
      },
    },
  },
}

return specs
