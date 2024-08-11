return {
   'nvim-lualine/lualine.nvim',
   enabled=vim.g.liteMode == 0,
   cond = vim.g.vscode ~= 1,
   opts = function(_, current_opts)
      vim.o.showmode = false

      local short_mode_name = function(mode)
         local map = {
            ['NORMAL'] = 'NORM',
            ['INSERT'] = 'INS',
            ['VISUAL'] = 'VIS',
            ['V-BLOCK'] = 'V-BLOCK',
            ['V-LINE'] = 'V-LINE',
            ['REPLACE'] = 'REPL',
            ['SELECT'] = 'SEL',
            ['TERMINAL'] = 'TERM',
            ['COMMAND'] = 'CMD',
         }
         local out = map[mode]
         if out == nil then
            out = ""
         end
         return out
      end

      local function diff_source()
         local gitsigns = vim.b.gitsigns_status_dict
         if gitsigns then
            return {
               added = gitsigns.added,
               modified = gitsigns.changed,
               removed = gitsigns.removed
            }
         end
      end

      local opts = {
         options = {
            icons_enabled = vim.g.useNerdFont == 1 and IsPluginUsed('nvim-web-devicons'),
            theme = 'auto',
            component_separators = { left = '', right = ''},
            section_separators = { left = '', right = ''},
            disabled_filetypes = {
               statusline = {},
               winbar = {},
            },
            ignore_focus = {},
            always_divide_middle = true,
            globalstatus = false,
            refresh = {
               statusline = 1000,
               tabline = 1000,
               winbar = 1000,
            }
         },
         -- tabline = {
         --    lualine_a = {
         --       {
         --          'buffers',
         --          show_filename_only = false, -- show shortened relative path
         --          mode = 4, -- Show buf name+number
         --          -- max_length = vim.o.columns * 4 / 5,
         --       },
         --    },
         --    lualine_z = {
         --       {'tabs', mode=2, max_length = vim.o.columns / 5},
         --    },
         -- },
         -- winbar = {},
         -- inactive_winbar = {},
         extensions = {},
      }

      if vim.g.usePLFont == 0 then
         opts.options.component_separators = { left = '', right = ''}
         opts.options.section_separators = { left = '', right = ''}
      end

      opts.sections = {
         lualine_a = {{ 'mode', fmt = function(str) return short_mode_name(str) end }},
         -- lualine_b = {
         --    -- 'branch',
         --    -- No point in multiple plugins getting head
         --    -- {'FugitiveHead', icon = ''},
         --    {'b:gitsigns_head', icon = ''},
         --    {'diff', source = diff_source},
         --    'diagnostics'},
         lualine_b = {
            { "branch"}
         },
         lualine_c = {
            {'encoding'},
            { "filetype", icon_only = opts.options.icons_enabled,
               separator = opts.options.icons_enabled and "" or opts.options.component_separators.left,
                  padding = { left = 1, right = 1 }
               },
            { "filename", path = 1, shorting_target = vim.o.columns / 2.5,
               symbols = { modified = "  ", readonly = " 🔒", unnamed = "" },
               separator = opts.options.section_separators.left,
               padding = { left = 0, right = 1},
            },
         },
         -- lualine_x = {},
         lualine_y = {'searchcount', },
         lualine_z = {
            '%p%%(%l/%L:%c)'
         }
      }
      opts.inactive_sections = {
         lualine_a = {},
         lualine_b = {},
         lualine_c = {'filename'},
         lualine_x = {'location'},
         lualine_y = {},
         lualine_z = {}
      }

      -- Swap around two sections
      if current_opts.section then
         for i, elem in ipairs(current_opts.sections.lualine_x) do
            if elem[1] == "diff" then
               table.insert(opts.sections.lualine_b, elem)
               current_opts.sections.lualine_x[i] = nil
               break
            end
         end
         for i, elem in ipairs(current_opts.sections.lualine_c) do
            if elem[1] == "diagnostics" then
               table.insert(opts.sections.lualine_y, 1, elem)
               current_opts.sections.lualine_c[i] = nil
               break
            end
         end
      end

      if vim.g.usePLFont == 0 then
         opts.sections.lualine_b[1].icon = nil
      end


      local used_extensions = {
         ['nvim-dap-ui'] = 'nvim-dap-ui',
         ['fugitive'] = 'fugitive',
      }
      for plugin, extension in pairs(used_extensions) do
         if IsPluginUsed(plugin) then
            table.insert(opts.extensions, extension)
         end
      end

      vim.api.nvim_create_user_command('LualineHide', require('lualine').hide,
      { force = true, desc = 'Hide lualine'})

      vim.api.nvim_create_user_command('LualineShow',
      function() require('lualine').hide({unhide=true}) end,
      { force = true, desc = 'Unhide lualine'})

      opts = vim.tbl_deep_extend("force", current_opts, opts)
      return opts
   end,

   config = function(_, opts)
      require("lualine").setup(opts)
      vim.fn.SmallUIOnResize()
   end,
}
