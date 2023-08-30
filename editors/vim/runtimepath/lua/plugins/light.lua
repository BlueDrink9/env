return {
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
    -- " Plugin 'https://github.com/gbprod/yanky.nvim'
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
