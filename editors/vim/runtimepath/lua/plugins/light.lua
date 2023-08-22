return {
  {"substitute.nvim",
    keys = {
      {mode="n", "cx", require('substitute.exchange').operator},
      {mode="n", "cxx", require('substitute.exchange').line},
      {mode="x", "X", require('substitute.exchange').visual},
      {mode="n", "cxc", require('substitute.exchange').cancel},
    }
  },

  {"eyeliner.nvim", opts = { highlight_on_key = true, dim = true }},

}
