-- call SourcePluginFile("nvim-lspconfig.lua")
    -- call SourcePluginFile('nvim-cmp.lua')

if vim.g.ideMode==0 then
  return {}
end

local idemaps = vim.g.IDE_mappings

return {


  -- Dependency for a lot of plugins
  {'nvim-lua/plenary.nvim'},

  -- For installing LSPs (and other packages)
  {'williamboman/mason.nvim'},
  {'https://github.com/RubixDev/mason-update-all'},
  {'https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim'},

  {'https://github.com/norcalli/nvim-colorizer.lua', config=true},
  -- {'nvim-notify', opt={stages="static"}},

  -- {'mason',
  -- require("mason").setup({
  --       ui = {
  --          icons = {
  --             package_installed = "✓",
  --             package_pending = "➜",
  --             package_uninstalled = "✗"
  --          }
  --       }
  --    })
  -- require('mason-update-all').setup()
  -- require('mason-tool-installer').setup()
  -- -- nvim --headless -c 'autocmd User MasonUpdateAllComplete quitall' -c 'MasonUpdateAll'
  -- },

  -- {[} ---------- Visual ----------
  {'folke/noice.nvim',
    enabled=false,
    opts={
      cmdline = {
        -- view = "cmdline",  -- or "cmdline_popup" for fancy.
        -- Sent to nui.nvim's input popup
        input = {
          -- input:map("n", "<esc>", input.input_props.on_close, { noremap = true })
        }
      },
      popupmenu = {
        enabled = true,
      },
      presets = {
        command_palette = true,
        long_message_to_split = true,
      },
      messages = {
        view_search = false,
      },
      lsp = {
        progress = {
          enabled = false,
        }
      }
    }
  },
  -- override my normal mapping to ctrl-f, since noice can handle it like normal
  -- vim.api.nvim_create_autocmd(
  -- "VimEnter",
  --  {
  --   group = "myIDE",
  --   buffer = bufnr,
  --   callback = function()
  --      vim.api.nvim_set_keymap('c', "kv", "<esc>", { noremap=true, silent=true })
  --      vim.api.nvim_set_keymap('c', "vk", "<esc>", { noremap=true, silent=true })
  --   end,
  --  })

  {'https://github.com/lukas-reineke/indent-blankline.nvim',
    init = function()
      local indentcolours = {}
      if vim.opt.termguicolors then
        -- Auto-gen some greys
        for i=100,201,(200-100)/5 do
          table.insert(indentcolours, string.format("#%02x%02x%02x", i, i, i))
        end
      else
        indentcolours = {
          "Red",
          "Yellow",
          "Green",
          "Blue",
          "Purple",
        }
      end
      for i, colour in pairs(indentcolours) do
        local cmd = "IndentBlanklineIndent"..i.." guifg="..colour.." gui=nocombine"
        vim.cmd('call add(g:customHLGroups, "'.. cmd .. '")')
      end
    end,

    opts = {
      char = "┆",
      char_blankline = "",
      show_first_indent_level = false,
      char_highlight_list = {
        "IndentBlanklineIndent1",
        "IndentBlanklineIndent2",
        "IndentBlanklineIndent3",
        "IndentBlanklineIndent4",
        "IndentBlanklineIndent5",
      },
      indent_blankline_use_treesitter = vim.fn.IsPluginUsed('nvim-treesitter') == 1,
      -- May be a touch slow
      show_current_context = vim.fn.IsPluginUsed('nvim-treesitter') == 1,
      show_current_context_start = not vim.fn.IsPluginUsed('nvim-treesitter') == 1,
      indent_blankline_show_current_context_start_on_current_line = not vim.fn.IsPluginUsed('nvim-treesitter') == 1,
    },
  },

  {'https://github.com/folke/which-key.nvim',
    opts = {
      triggers_blacklist = {
        i = { "k", "v", "(", "{", "[", },
      },
    },
    event='VeryLazy',
  },

  {'sudormrfbin/cheatsheet.nvim',
    cmd='Cheatsheet',
    keys={"<leader><s-/>", "<cmd>Cheatsheet<cr>"},
  },

  {'https://github.com/winston0410/range-highlight.nvim',
    dependencies = {'winston0410/cmd-parser.nvim'},
    event='VeryLazy',
    config=true,
  },

  -- {'mfussenegger/nvim-lint', config = function()
  --   vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  --     callback = function()
  --       require("lint").try_lint()
  --     end,
  --     group='myIDE',
  --   })
  -- end
  -- },

  -- {]} ---------- Visual ----------

  -- -- " Haven't configured yet.
  -- -- DEPRECATED - TODO REMOVE
  -- {'jose-elias-alvarez/null-ls.nvim',
  --   -- will setup any installed and configured sources for mason
  --   opts = {
  --     sources = {
  --       -- null_ls.builtins.code_actions.refactoring,
  --       require'null_ls'.builtins.completion.spell,
  --       require'null_ls'.builtins.hover.printenv,
  --     }
  --   }
  -- },

  -- {'jayp0521/mason-null-ls',
  --   opts = {
  --     ensure_installed = {
  --       'vint',
  --       'luacheck',
  --       'stylua',
  --       'pylint',
  --       'shellcheck',
  --       'jq',
  --       'proselint',
  --     },
  --     automatic_setup = true,
  --   },
  --   config = require'mason-null-ls'.setup_handlers,
  -- },

  -- {'simrat39/symbols-outline.nvim', config=true},

  {'https://github.com/TimUntersberger/neogit',

    config=function()
      vim.api.nvim_create_user_command('Magit', 'Neogit')
    end,
    cmd={"Neogit"},
    keys={'<leader>gg', '<cmd>Neogit<CR>', desc='NeoGit'},
  },

  -- {[} ---------- REPL ----------
  -- {"michaelb/sniprun",
  --   opts = {
  --     --" you can combo different display modes as desired
  --     display = {
  --       "Classic",                    -- "display results in the command-line  area
  --       "VirtualTextOk",              -- "display ok results as virtual text (multiline is shortened)

  --       "VirtualTextErr",          -- "display error results as virtual text
  --       -- "TempFloatingWindow",      -- "display results in a floating window
  --       "LongTempFloatingWindow",  -- "same as above, but only long results. To use with VirtualText__
  --       -- "Terminal"                 -- "display results in a vertical split
  --       "TerminalWithCode",        --# display results and code history in a vertical split
  --     },
  --   },
  --   keys = {
  --     {idemaps.REPLSendLine, '<Plug>SnipRun'},
  --     {idemaps.REPLSend, '<Plug>SnipRunOperator'},
  --     {idemaps.REPLCancel, '<Plug>SnipReset'},
  --     {idemaps.REPLClear, '<Plug>SnipClose'},
  --     {idemaps.REPLClose, '<Plug>SnipClose'},
  --     {idemaps.REPLSend, '<Plug>SnipRun', mode='v'},
  --     -- sniprunfile_keep_position
  --     {idemaps.REPLSendFile, [[
  --     <cmd>let b:caret=winsaveview()
  --          <bar> %SnipRun
  --          <bar> call winrestview(b:caret)<CR>
  --       ]]
  --     },
  --   },
  -- },

  {"https://github.com/hkupty/iron.nvim",
    opts = function()
      return {
        config = {
          -- Whether a repl should be discarded or not
          scratch_repl = true,
          repl_definition = {
            sh = {
              command = {"bash"}
            }
          },
          repl_open_cmd = require'iron.view'.split.horizontal.belowright(0.15),
        },
        keymaps = {
          send_motion = idemaps.REPLSend,
          visual_send = idemaps.REPLSend,
          send_file = idemaps.REPLSendFile,
          send_line = idemaps.REPLSendLine,
          -- send_mark = idemaps.REPLSend,
          -- mark_motion = idemaps.REPLSend,
          -- mark_visual = idemaps.REPLSend,
          -- remove_mark = idemaps.REPLSend,
          -- cr = idemaps.REPLSend,
          interrupt = idemaps.REPLCancel,
          exit = idemaps.REPLClose,
          clear = idemaps.REPLClear,
        },
        highlight = {
          italic = true
        },
        ignore_blank_lines = false,
      }
    end,
    keys = {
      {'<space>s<c-s>', '<cmd>IronFocus<cr>'}
    }
  },

  -- {]} ---------- REPL ----------

  {'jalvesaq/Nvim-R',
    config = function()
      vim.fn.SourcePluginFile("nvim-R.vim")
    end
  },

  {
    'https://github.com/hrsh7th/vim-vsnip',
    keys={
      {idemaps.snippetExpand, '<Plug>(vsnip-expand-or-jump)', mode={'i', 'v'}},
    },
    config = function()
      vim.g.vsnip_snippet_dir = vim.fn.PathExpand(vim.g.plugin_config_dir .. '/../runtimepath/snippets')
    end,
    event='InsertEnter'
  },

  {'hrsh7th/vim-vsnip-integ',
    event='InsertEnter'},

  {'octaltree/virtualsnip',
    build='make',
    event='InsertEnter'},

}