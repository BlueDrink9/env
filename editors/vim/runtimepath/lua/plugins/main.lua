if vim.g.liteMode == 1 then
  return {}
end

return {

	{ "andymass/vim-matchup", event="VeryLazy"},

  {
    'echasnovski/mini.align',
    opts = {
      mappings = { start = '', start_with_preview = 'gA' }
    },
    cmd="Align",
    keys={"gA"},
    init=function()
      vim.api.nvim_create_user_command(
        'Align', function() require('mini.align').action_visual(false) end, {})
    end,
  },

  {"https://github.com/NMAC427/guess-indent.nvim", event="VeryLazy", config=true},

  {"https://github.com/stevearc/dressing.nvim.git",
    cond=vim.g.vscode ~= 1,
  event="VeryLazy"},

  {'https://github.com/ggandor/leap.nvim.git',
    config = function()
      -- Will not override existing mappings.
      require('leap').add_default_mappings()
    end,
    opts = {
      prev_target = {'<tab>',}  -- Remove ',' from the default
    },
    keys = {'s', 'S'},
    -- Can't really figure out how to use this one atm.
    -- if vim.fn.IsPluginUsed('leap-ast.nvim') == 1 and vim.fn.IsPluginUsed('nvim-treesitter') == 1 then
    --     vim.keymap.set(modes, ',W',
    --     function() require'leap-ast'.leap() end,
    --     {desc='leap to ast elemenjt'})
    -- end
  },
  {'https://github.com/ggandor/leap-spooky.nvim',
    config=true,
    keys = function()
      local out = {}
      for _, key1 in ipairs({'i', 'a'}) do
        for _, key2 in ipairs({'r', 'R', 'm', 'M'}) do
          table.insert(out, {key1 .. key2, mode='o'})
        end
      end
      return out
    end
  },
  -- {'https://github.com/ggandor/leap-ast.nvim'},

  {"https://github.com/Nguyen-Hoang-Nam/nvim-preview-csv",
    -- Want to keep just in case we want the movement as well as the view
    init = function()
      vim.g.csv_autocmd_arrange = 0
    end,
    opts = {
      max_csv_line = 100
    },
    ft="csv",
  },



  {
    'https://github.com/lewis6991/gitsigns.nvim.git',
    event='VeryLazy',
    cond = vim.g.vscode ~= 1,
    on_attach = function(buffer)
      local gs = package.loaded.gitsigns
      local function map(mode, l, r, desc)
        vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
      end
      -- stylua: ignore start
      map("n", "]h", gs.next_hunk, "Next Hunk")
      map("n", "[h", gs.prev_hunk, "Prev Hunk")
      map({ "n", "v" }, "<leader>gs", ":Gitsigns stage_hunk<CR>", "Stage Hunk")
      map({ "n", "v" }, "<leader>ghr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
      map("n", "<leader>gS", gs.stage_buffer, "Stage Buffer")
      map("n", "<leader>gu", gs.undo_stage_hunk, "Undo Stage Hunk")
      map("n", "<leader>gR", gs.reset_buffer, "Reset Buffer")
      map("n", "<leader>gp", gs.preview_hunk, "Preview Hunk")
      map("n", "<leader>ghb", function() gs.blame_line({ full = true }) end, "Blame Line")
      map("n", "<leader>ghd", gs.diffthis, "Diff This")
      map("n", "<leader>ghD", function() gs.diffthis("~") end, "Diff This ~")
      map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
    end,
    opts={
      -- signs = {
      --   add = {hl = 'GitSignsAdd', text = '+', numhl='GitSignsAddNr', linehl='GitSignsAddLn'},
      -- },
      signcolumn = true,
      numhl      = true,
    }
  },

  {"https://github.com/akinsho/toggleterm.nvim",
    cond = vim.g.vscode ~= 1,
    keys = "<C-s>",
    opts = function()
      opts ={
        open_mapping = [[<c-s>]],
        direction = 'horizontal',
        start_in_insert = false,
        insert_mappings = false,
        terminal_mappings = true,
        persist_mode = true,
        close_on_exit = false, -- Otherwise may miss startup errors
        auto_scroll = false,
        shell = vim.o.shell,
      }
      if vim.fn.has("win32") == 1 then
        opts.shell = "powershell.exe"
      end
      return opts
    end
    -- :ToggleTermSendVisualSelection
  },


  -- Extra filetypes

  {
    'https://github.com/salkin-mada/openscad.nvim',
    ft='scad',
  },

  {
    'https://github.com/nvim-orgmode/orgmode',
    ft='org',
  },


  {
    -- Has come up with a decent set of low-clash bindings I think.
    'julienvincent/nvim-paredit',
    ft={
      -- Currently only supports closure :/
      -- 'commonlisp',
      -- 'elisp',
      'closure',
    },
    opts = {
      indent = {
        enabled = true,
      }
    },
  },

  {
      "chrishrb/gx.nvim",
      keys = {"gx"},
      dependencies = { "nvim-lua/plenary.nvim" },
      opts = {
          handler_options = {
              search_engine = "duckduckgo", -- you can select between google, bing, duckduckgo, and ecosia
          },
      },
  },

  {
    -- Mainly taken from lazyvim, but leaving out any IDE and lazy bits.
    -- Lazyvim can add them back in when in IDE mode.
    "akinsho/bufferline.nvim",
    cond = vim.g.vscode ~= 1,
    event = "VeryLazy",
    keys = {
      { "<leader>bp", "<Cmd>BufferLineTogglePin<CR>", desc = "Toggle pin" },
      { "<leader>bP", "<Cmd>BufferLineGroupClose ungrouped<CR>", desc = "Delete non-pinned buffers" },
    },
    opts = function(_, opts)
      opts.options = opts.options or {}
      opts.options.numbers = "buffer_id"
      opts.options.close_command = function(n)
        require("mini.bufremove").delete(n, false) end
      opts.options.right_mouse_command = function(n)
        require("mini.bufremove").delete(n, false) end
      opts.options.always_show_bufferline = false
    end
  },

  {
    "vscode-neovim/vscode-neovim",
    cond = vim.g.vscode == 1,
    config = function(_, opts)
      vim.fn.SourcePluginFile("vscode-neovim.vim")
    end
  },

  {
    'tzachar/highlight-undo.nvim',
    config=true,
    keys = {"u", "<C-r>" }
  },

  {
    "sindrets/diffview.nvim",
    config=true,
    cmd = {
      "DiffviewFileHistory",
      "DiffviewOpen",
    },
    event="OptionSet diff",
    lazy=not vim.o.diff,
  },

  {"kwkarlwang/bufresize.nvim", event="VeryLazy"},

}
