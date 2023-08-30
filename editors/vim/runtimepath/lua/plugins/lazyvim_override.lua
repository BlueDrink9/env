-- if  require'lazy'.opts.spec['LazyVim/LazyVim'] == nil then
--   return {}
-- end


-- surround mappings
-- replace
-- exchange
-- arg swap
-- persistance/session management replace
-- mason - delay installs/don't ensure installed
-- space c replacement/remap
-- Rg
-- Fugitive + neogit
-- Windowtitle
-- cmp: don't select first one automatically
-- Startup

return {

  {
    enabled = false,
    "folke/noice.nvim",
    -- opts will be merged with the parent spec
    opts = {
      messages = { enabled = false },
      cmdline = { enabled = false },
    },
    config = function()
      vim.cmd('echom noice loaded')
      vim.opt.lazyredraw = false
    end,
    init = function()
      vim.opt.lazyredraw = false
    end,
    event="VeryLazy",
  },

  {
    "nvim-treesitter/nvim-treesitter",
    enabled = vim.g.ideMode == 1,
  },

  {
    "echasnovski/mini.surround",
    enabled=false,
    opts = {
      mappings = {
        add = "ys",
        delete = "ds",
        find = "ysf",
        find_left = "ysF",
        highlight = "ysh",
        replace = "ysr",
        update_n_lines = "ysn",
      },
    },
  },

  {
    "echasnovski/mini.comment",
    enabled=false,
    opts = {
      mappings = {
        comment = "<leader>c",
        comment_line = "<leader>cc",
        textobject = "ac",
      },
    },
  },

  {
    "echasnovski/mini.ai",
    opts = {
      custom_textobjects = {
        -- Replace class key with C so that comment can be c
        C = function()
          return require("mini.ai").config.custom_textobjects.c
        end,
      },
    },
    keys = {
      {"a", mode="o"},
      {"i", mode="o"},
    }
  },

  -- Use <tab> for completion and snippets (supertab)
  -- first: disable default <tab> and <s-tab> behavior in LuaSnip
  {
    "L3MON4D3/LuaSnip",
    keys = function()
      return {}
    end,
  },

  {
    "hrsh7th/nvim-cmp",
    enabled = vim.g.ideMode == 1;
    opts = function(_, opts)
      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end

      local luasnip = require("luasnip")
      local cmp = require("cmp")

      opts.mapping = cmp.mapping.preset.insert({
        ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
        ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<S-CR>"] = cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Replace,
          select = true,
        }),
        ["<C-e>"] = cmp.mapping(function(fallback)
          if luasnip.expand_or_locally_jumpable() then
            luasnip.expand_or_jump()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { "i", "s" }),
      })
    end,
  },

  {
    "jose-elias-alvarez/null-ls.nvim",
    enabled = vim.g.ideMode == 1,
    opts = function()
      local nls = require("null-ls")
      return {
        sources = {
          nls.builtins.formatting.stylua,
          nls.builtins.formatting.shfmt,
          -- nls.builtins.diagnostics.flake8,
        },
      }
    end,
  },

  {
    -- TODO figure out how ot change the alpha theme
    "goolord/alpha-nvim",
    enabled = false,
    opts = function()
      return require'alpha.themes.startify'
    end
  },
}
