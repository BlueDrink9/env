-- In your liteMode setup or settings:
if vim.g.liteMode == 1 then
  vim.o.showtabline = 2

  function _G.simple_buffer_tabline()
    local s = ""
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.bo[buf].buflisted and vim.api.nvim_buf_is_loaded(buf) then
        local name = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":t")
        name = (name == "" and "[No Name]" or name)
        local is_current = (buf == vim.api.nvim_get_current_buf())
        local modified = vim.bo[buf].modified and " +" or ""

        -- Highlight active vs inactive buffer tabs
        s = s .. (is_current and "%#TabLineSel#" or "%#TabLine#")
        s = s .. " " .. buf .. ":" .. name .. modified .. " "
      end
    end
    return s .. "%#TabLineFill#"
  end

  vim.o.tabline = "%!v:lua.simple_buffer_tabline()"
end

return {
  {
    "https://github.com/numToStr/Comment.nvim",
    enabled=vim.fn.has("nvim-0.10") == 0,
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
        visual_line = "gS",
      },
    },
    config = function(_, opts)

      -- custom surrounds
      local key_index_surround = {
        ["k"] = {
          add = quoted_surround,
          find = quoted_pattern,
          delete = quoted_pattern,
          change = {
            target = quoted_pattern,
          },
        },
        ["K"] = {
          add = unquoted_surround,
          find = unquoted_pattern,
          delete = unquoted_pattern,
          change = {
            target = unquoted_pattern,
          },
        },
      }

      local surrounds = {
        -- Keys = filetypes, then letter mapping for that surround.
        markdown = {
          -- create link from clipboard
          l = {
            add = function()
              local clipboard = vim.fn.getreg("+"):gsub("\n", "")
              return {
                { "[" },
                { "](" .. clipboard .. ")" },
              }
            end,
            find = "%b[]%b()",
            delete = "^(%[)().-(%]%b())()$",
            change = {
              target = "^()()%b[]%((.-)()%)$",
              replacement = function()
                local clipboard = vim.fn.getreg("+"):gsub("\n", "")
                return {
                  { "" },
                  { clipboard },
                }
              end,
            },
          },
        },
        latex = {
          -- Command and environment
          c = {
            add = function()
              local cmd = require("nvim-surround.config").get_input "Command: "
              return { { "\\" .. cmd .. "{" }, { "}" } }
            end,
          },
          e = {
            add = function()
              local env = require("nvim-surround.config").get_input "Environment: "
              return { { "\\begin{" .. env .. "}" }, { "\\end{" .. env .. "}" } }
            end,
          },
        },
        python = key_index_surround,
        lua = key_index_surround,
      }

      for filetype, letters in pairs(surrounds) do
        vim.api.nvim_create_autocmd({"FileType"}, {
          pattern = filetype,
          callback = function()
            require("nvim-surround").buffer_setup({
              surrounds = letters
            })
          end
        })
      end

      require("nvim-surround").setup(opts)
    end,
  },

  {"gbprod/substitute.nvim",
    keys = {
      {"cx", function() require('substitute.exchange').operator() end, mode="n"},
      {"cX", function() require('substitute.exchange').eol() end, mode="n"},
      {"cxx", function() require('substitute.exchange').line() end, mode="n"},
      {"cxc", function() require('substitute.exchange').cancel() end, mode="n"},
      {"cx<esc>", function() require('substitute.exchange').cancel() end, mode="n"},
      {"X", function() require('substitute.exchange').visual() end, mode="x"},
    },
    config=true,
  },

  -- Highlight f and t chars to get where you want.
  {'https://github.com/jinh0/eyeliner.nvim',
    opts = { highlight_on_key = true, dim = true },
    keys = {'f', 'F', 't', 'T'},
  },

  -- Replacing with lazyvim Yanky
  -- {
  --   'https://github.com/bfredl/nvim-miniyank',
  --   keys=function()
  --     local keys = {
  --       {'p', '<Plug>(miniyank-autoput)'},
  --       {'P', '<Plug>(miniyank-autoPut)'},
  --       {'<leader>p', '<Plug>(miniyank-cycle)'},
  --       {'<leader>P', '<Plug>(miniyank-cycleback)'},
  --     }
  --     for _, key in ipairs(keys) do
  --       key["mode"] = {'v', 'n'}
  --     end
  --     return keys
  --   end
  -- },

  {
    "itchyny/lightline.vim",
    lazy = false,
    disabled = false,
    cond = function()
      return vim.g.liteMode == 1
    end,
  },

}
