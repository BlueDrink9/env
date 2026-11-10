-- runtimepath/lua/plugins/treesitter.lua
return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    lazy = false, -- The 'main' rewrite does not support lazy-loading
    build = ":TSUpdate",
    config = function()
      local ts = require("nvim-treesitter")

      ts.setup()

      -- Manual parser installation command for the 'main' branch API
      vim.api.nvim_create_user_command("TSInstallMine", function()
        local parsers = {
          "bash", "c", "json", "lua", "make", "markdown", "markdown_inline",
          "python", "query", "r", "regex", "sql", "toml", "vim", "vimdoc", "yaml",
        }
        ts.install(parsers)
      end, { desc = "Install personal set of Treesitter parsers" })

      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("NativeTreesitterAttach", { clear = true }),
        callback = function(args)
          local lang = vim.treesitter.language.get_lang(vim.bo[args.buf].filetype)
          if lang and not pcall(vim.treesitter.start, args.buf, lang) then
            -- Parser unavailable or compiling; fallback to standard syntax
          end
          vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
          vim.wo.foldmethod = "expr"
        end,
      })

      -- Core AST sibling movement using native TSNode traversal
      vim.keymap.set("n", "],", function()
        local node = vim.treesitter.get_node()
        if node and node:next_sibling() then
          local row, col = node:next_sibling():start()
          vim.api.nvim_win_set_cursor(0, { row + 1, col })
        end
      end, { desc = "Next AST Sibling Node" })

      vim.keymap.set("n", "[,", function()
        local node = vim.treesitter.get_node()
        if node and node:prev_sibling() then
          local row, col = node:prev_sibling():start()
          vim.api.nvim_win_set_cursor(0, { row + 1, col })
        end
      end, { desc = "Previous AST Sibling Node" })
    end,
  },

  -- Keep textobjects module on the 'main' branch
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      local select = require("nvim-treesitter-textobjects.select")
      local swap = require("nvim-treesitter-textobjects.swap")

      vim.keymap.set({ "x", "o" }, "af", function() select.select_textobject("@function.outer", "textobjects") end)
      vim.keymap.set({ "x", "o" }, "if", function() select.select_textobject("@function.inner", "textobjects") end)
      vim.keymap.set({ "x", "o" }, "ac", function() select.select_textobject("@class.outer", "textobjects") end)
      vim.keymap.set({ "x", "o" }, "ic", function() select.select_textobject("@class.inner", "textobjects") end)

      vim.keymap.set("n", ">,", function() swap.swap_next("@parameter.inner") end, { desc = "Swap Next Parameter" })
      vim.keymap.set("n", "<,", function() swap.swap_previous("@parameter.inner") end, { desc = "Swap Prev Parameter" })
    end,
  },
}
