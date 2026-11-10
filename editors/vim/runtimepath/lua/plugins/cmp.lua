-- runtimepath/lua/plugins/cmp.lua
return {
  -- 1. Mini.snippets handles runtimepath snippet discovery and tabstop indicators
  {
    "nvim-mini/mini.snippets",
    event = "InsertEnter",
    dependencies = { "rafamadriz/friendly-snippets" },
    opts = function()
      local mini_snippets = require("mini.snippets")
      return {
        snippets = {
          -- Discovers snippets/ in every runtimepath plugin
          mini_snippets.gen_loader.from_lang(),
        },
        mappings = {
          jump_next = "", -- Contextually bound to <C-e> in blink.cmp
          jump_prev = "",
          stop = "<Esc>",
        },
      }
    end,
    config = function(_, opts)
      require("mini.snippets").setup(opts)

      -- End snippet session on leaving insert mode to prevent cursor jumping
      vim.api.nvim_create_autocmd("ModeChanged", {
        pattern = "*:n",
        callback = function()
          local ms = package.loaded["mini.snippets"]
          if ms and ms.session.get() then
            ms.session.stop()
          end
        end,
        desc = "Terminate snippet session on entering Normal mode",
      })
      vim.api.nvim_create_user_command("SnipEdit", function()
        local snip_dir = vim.fn.stdpath("config") .. "/runtimepath/snippets"
        local target = snip_dir .. "/" .. vim.bo.filetype .. ".json"
        vim.cmd.edit(target)
      end, { desc = "Edit snippet file for current filetype" })
    end,
  },

  -- 2. Blink.cmp autocompletion engine
  {
    "saghen/blink.cmp",
    version = "*",
    event = "InsertEnter",
    dependencies = {
      "nvim-mini/mini.snippets",
    },
    opts = function(_, opts)
      vim.g.cmptoggle = true
      -- Create a quick way to toggle cmp
      local toggle_cmp = function()
        vim.g.cmptoggle = not vim.g.cmptoggle
        print("Autocompletion " .. (vim.g.cmptoggle and "Enabled" or "Disabled"))
      end
      vim.keymap.set("n", "<leader>tC", toggle_cmp, { desc = "Toggle Completion (Blink)" })
      vim.api.nvim_create_user_command("CmpToggle", toggle_cmp, {})

      -- Include all visible buffers (not just default of current one) below a certain size.
      local get_visible_small_buffers = function()
        local bufs = {}
        local max_size = 1024 * 1024 -- 1 Megabyte max
        for _, win in ipairs(vim.api.nvim_list_wins()) do
          local buf = vim.api.nvim_win_get_buf(win)
          local line_count = vim.api.nvim_buf_line_count(buf)
          local byte_size = vim.api.nvim_buf_get_offset(buf, line_count)
          if byte_size < max_size then
            bufs[buf] = true
          end
        end
        return vim.tbl_keys(bufs)
      end

      return vim.tbl_deep_extend("force", opts or {}, {
        enabled = function()
          return vim.g.cmptoggle and vim.g.ideMode == 1 and vim.bo.buftype ~= "prompt"
        end,

        snippets = {
          preset = "mini_snippets",
        },

        completion = {
          list = {
            selection = {
              preselect = false,
              auto_insert = false,
            },
          },
          menu = {
            border = "rounded",
            auto_show = function(ctx) return ctx.mode ~= "cmdline" end,
          },
          documentation = {
            auto_show = true,
            auto_show_delay_ms = 200,
            window = { border = "rounded" },
          },
        },

        signature = {
          enabled = true,
          window = { border = "rounded" },
        },

        keymap = {
          -- Setting up mappings for supertab-like behaviour
          preset = "super-tab",
          ["<Tab>"] = { "select_next", "fallback" },
          ["<S-Tab>"] = { "select_prev", "fallback" },

          -- Contextual <C-e>: jumps active snippet or confirms completion
          ["<C-e>"] = {
            function(cmp)
              local ms = package.loaded["mini.snippets"]
              if ms and ms.session.get() then
                ms.session.jump("next")
                return true
              elseif cmp.is_visible() then
                return cmp.accept()
              end
            end,
            "fallback",
          },

          ["<S-CR>"] = {
            function(cmp)
              return cmp.accept({ behavior = "replace" })
            end,
            "fallback",
          },
        },

        sources = {
          default = { "lsp", "path", "snippets", "buffer" },
          providers = {
            buffer = {
              name = "Buffer",
              module = "blink.cmp.sources.buffer",
              opts = {
                get_bufnrs = get_visible_small_buffers,
              },
            },
          },
        },
      })
    end,
  },
}
