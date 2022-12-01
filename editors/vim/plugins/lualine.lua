local location_format = ''
local config = {
   options = {
      icons_enabled = true,
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
   sections = {
      lualine_a = {'mode'},
      lualine_b = {'branch', 'diff', 'diagnostics'},
      lualine_c = {{'filename', path = 1, shorting_target = vim.o.columns / 2.5}},
      lualine_x = {'encoding', 'fileformat', 'filetype'},
      lualine_y = {'progress'},
      lualine_z = {'searchcount', 'location'}
   },
   inactive_sections = {
      lualine_a = {},
      lualine_b = {},
      lualine_c = {'filename'},
      lualine_x = {'location'},
      lualine_y = {},
      lualine_z = {}
   },
   tabline = {
      lualine_a = {
         {
            'buffers',
            show_filename_only = false, -- show shortened relative path
            mode = 4, -- Show buf name+number
            -- max_length = vim.o.columns * 4 / 5,
         },
      },
      lualine_z = {
         {'tabs', mode=2, max_length = vim.o.columns / 5},
      },
   },
   winbar = {},
   inactive_winbar = {},
   extensions = {},
}

if not vim.g.useNerdFont or not vim.fn.IsPluginUsed('nvim-web-devicons') then
   config.options.icons_enabled = false
end
if not vim.g.usePLFont then
   config.options.component_separators = { left = '', right = ''}
   config.options.section_separators = { left = '', right = ''}
end

local used_extensions = {
   ['nvim-dap-ui'] = 'nvim-dap-ui',
   ['fugitive'] = 'fugitive',
}
for plugin, extension in pairs(used_extensions) do
   if vim.fn.IsPluginUsed(plugin) then
      table.insert(config.extensions, extension)
   end
end

require('lualine').setup(config)


vim.api.nvim_create_user_command('LualineHide', require('lualine').hide,
   { force = true, desc = 'Hide lualine'})

vim.api.nvim_create_user_command('LualineShow',
   function() require('lualine').hide({unhide=true}) end,
   { force = true, desc = 'Unhide lualine'})

