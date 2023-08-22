return {
  {
    "Comment.nvim",
    -- xnoremap <leader>gc  :norm! <leader>cc<CR>
    opts = {
      ---Lines to be ignored while (un)comment
      ignore = '^%s*$',
      ---LHS of toggle mappings in NORMAL mode
      toggler = {
        line = '<leader>cc',
        block = '<leader>cC',
      },
      opleader = {
        line = '<leader>c',
        block = '<leader>C',
      },
      mappings = {
        basic = true,
        extra = false,
      },
      pre_hook = nil,
      post_hook = nil,
    },
    keys = {'<leader>c', '<leader>C'},
  },

  {
    "mini.align",
    opts = {
      mappings = { start = '', start_with_preview = 'gA' }
    },
    on="Align",
    init=function()
      vim.api.nvim_create_user_command(
        'Align', function() require('mini.align').action_visual(false) end, {})
    end,
  },

  {"guess-indent.nvim", config=true},

  {"dressing.nvim", config=true},

  {'leap.nvim',
    config = function()
      -- Will not override existing mappings.
      require('leap').add_default_mappings()
    end,
    opts = {
      prev_target = {'<tab>',}  -- Remove ',' from the default
    }
    -- Can't really figure out how to use this one atm.
    -- if vim.fn.IsPluginUsed('leap-ast.nvim') == 1 and vim.fn.IsPluginUsed('nvim-treesitter') == 1 then
    --     vim.keymap.set(modes, ',W',
    --     function() require'leap-ast'.leap() end,
    --     {desc='leap to ast element'})
    -- end
  },

  {"vim-openscad",
    config=true,
    init = function()
      vim.g.openscad_load_snippets = false
      vim.g.openscad_auto_open = false
      vim.g.openscad_default_mappings = false
    end
  },

  {"nvim-preview-csv",
    -- Want to keep just in case we want the movement as well as the view
    init = function()
      vim.g.csv_autocmd_arrange	   = 0
    end,
    opts = {
      max_csv_line = 100
    },
  },



  {
    'gitsigns.nvim',
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

  {"toggleterm.nvim",
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

  {
    "https://github.com/glacambre/firenvim",
    lazy = not vim.g.started_by_firenvim,
    module = false,
    build = function()
        -- " if has('win32')
        -- "     let s:firenvim_startup_prologue='"set LITE_SYSTEM=1"'
        -- " else
        -- "     let s:firenvim_startup_prologue='"export LITE_SYSTEM=1"'
        -- " endif
        -- let s:firenvim_startup_prologue=''
        -- let g:firenvim_install=":call firenvim#install(0, " . s:firenvim_startup_prologue . ")"
      vim.fn["firenvim#install"](0)
    end,
  }

}
