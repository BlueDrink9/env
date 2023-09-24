return {
  {
    "https://github.com/numToStr/Comment.nvim",
    opts = {
      -- Lines to be ignored while (un)comment
      ignore = '^%s*$',
      -- LHS of toggle mappings in NORMAL mode
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
    keys = {
      {'<leader>c', mode={'n', 'x'}}, '<leader>C'
    },
  },

  {
    "kylechui/nvim-surround",
    version = "*",
    keys = { 'ys', 'ds', 'cs', {'S', mode={'x'}}, },
    opts = {
      keymaps = {
        insert = nil,
        insert_line = nil,
        visual = "S",
        visual_line = "S",
      },
    },
  },

  {"gbprod/substitute.nvim",
    keys = {
      {"cx", function() require('substitute').operator() end, mode="n"},
      {"cX", function() require('substitute').eol() end, mode="n"},
      {"cxx", function() require('substitute').line() end, mode="n"},
      {"cxc", function() require('substitute').cancel() end, mode="n"},
      {"X", function() require('substitute').visual() end, mode="x"},
    }
  },

  -- Highlight f and t chars to get where you want.
  {'https://github.com/jinh0/eyeliner.nvim',
    opts = { highlight_on_key = true, dim = true },
    keys = {'f', 'F', 't', 'T'},
  },

  {
    'https://github.com/bfredl/nvim-miniyank',
    keys=function()
      local keys = {
        {'p', '<Plug>(miniyank-autoput)'},
        {'P', '<Plug>(miniyank-autoPut)'},
        {'<leader>p', '<Plug>(miniyank-cycle)'},
        {'<leader>P', '<Plug>(miniyank-cycleback)'},
      }
      for _, key in ipairs(keys) do
        key["mode"] = {'v', 'n'}
      end
      return keys
    end
  },

}
