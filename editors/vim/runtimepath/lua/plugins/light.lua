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

}
