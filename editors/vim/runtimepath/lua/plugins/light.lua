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

  {"eyeliner.nvim", opts = { highlight_on_key = true, dim = true }},

}
