return {
  {"substitute.nvim",
    keys = {
      {"cx", function() require('substitute.exchange').operator() end, mode="n"},
      {"cxx", function() require('substitute.exchange').line() end, mode="n"},
      {"X", function() require('substitute.exchange').visual() end, mode="x"},
      {"cxc", function() require('substitute.exchange').cancel() end, mode="n"},
    }
  },

  {"eyeliner.nvim", opts = { highlight_on_key = true, dim = true }},

}
