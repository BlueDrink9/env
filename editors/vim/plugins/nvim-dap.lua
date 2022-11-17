require("mason").setup()
local dap = require("dap")
local mason_dap = require("mason-nvim-dap")
mason_dap.setup()
local dapui = require("dapui")

require("nvim-dap-virtual-text").setup {
   commented = true,
}


local maps = vim.g.IDE_mappings
-- Mappings.
-- See `:help vim.lsp.*` for documentation on any of the below functions
local dap_nbufmaps = {
   [maps.debugFile] = 'continue()',
   [maps.debugReset] = 'terminate()',
   [maps.debugContinue] = 'continue()',
   [maps.debugRestart] = 'run_last()',
   [maps.debugStepOver] = 'step_over()',
   [maps.debugStepInto] = 'step_into()',
   [maps.debugStepOut] = 'step_out()',
   [maps.debugStepBack] = 'step_back()',
   [maps.debugRunToHere] = 'run_to_cursor()',
   [maps.debugInspect] = 'repl.toggle()',
   [maps.debugFrameUp] = 'up()',
   [maps.debugFrameDown] = 'down()',
   [maps.setBreakpoint] = 'toggle_breakpoint()',
   [maps.setBreakpointConditional] = "set_breakpoint(vim.fn.input('Breakpoint condition: ')",
   [maps.breakpointList] = 'list_breakpoints()',
   -- [debugShowOutput] = '',
   -- [debugHover] = '',
}
local dapui_nbufmaps = {
   -- [vnoremap_M-k]  = 'eval()',
-- require("dapui").toggle()
}


mason_dap.setup({
      -- ensure_installed = {'stylua', 'jq'}
   })

-- Ensures mappings are only set for filetypes with installed DAPs
local set_up_buffer = function()
   vim.cmd[[command! -buffer Debug lua require("dapui").toggle()]]
   --
   -- Mappings.
   local bufnr = vim.api.nvim_buf_get_number(0)
   local prefixes = {dap="require'dap'", ui="require'dap-ui'"}
   local to_map_tables = {dap=dap_nbufmaps, ui=dapui_nbufmaps}
   for prefix, table in pairs(to_map_tables) do
      for mapping, cmd in pairs(table) do
         local rhs = "<cmd>lua " .. prefixes[prefix] .. "." .. cmd .. "<CR>"
         vim.api.nvim_buf_set_keymap(bufnr, 'n', mapping, rhs,
            { noremap=true, silent=true })
      end
   end
end

local set_up_autocmd = function(source_name)
   vim.api.nvim_create_autocmd("Filetype", {
         group = "myIDE",
         pattern = source_name,
         callback = set_up_buffer,
      })
end

mason_dap.setup_handlers {
   function(source_name)
      set_up_autocmd(source_name)
      -- dap.listeners.after.event_initialized["dap"] = on_attach(vim.api.nvim_buf_get_number)
   end,
   python = function()
      set_up_autocmd("python")
      require('dap-python').setup(vim.g.python3_host_prog)
      vim.cmd[[command! -buffer DebugTestMethod lua require('dap-python').test_method()]]
      vim.cmd[[command! -buffer DebugTestClass lua require('dap-python').test_class()]]

      dap.adapters.python = {
         type = "executable",
         -- command = "/usr/bin/python3",
         command = "python3",
         args = {
            "-m",
            "debugpy.adapter",
         },
      }

      -- Insert because dap-python already configures it
      table.insert(dap.configurations.python, {
         {
            type = "python",
            request = "launch",
            name = "Launch file",
            program = "${file}", -- This configuration will launch the current file if used.
            },
         })
      end,
   }



dapui.setup({
  icons = { expanded = "▾", collapsed = "▸", current_frame = "▸" },
  mappings = {
    -- Use a table to apply multiple mappings
    expand = { "<CR>", "<2-LeftMouse>" },
    open = "o",
    remove = "d",
    edit = "e",
    repl = "r",
    toggle = "t",
  },
  -- Expand lines larger than the window
  -- Requires >= 0.7
  expand_lines = vim.fn.has("nvim-0.7") == 1,
  -- Layouts define sections of the screen to place windows.
  -- The position can be "left", "right", "top" or "bottom".
  -- The size specifies the height/width depending on position. It can be an Int
  -- or a Float. Integer specifies height/width directly (i.e. 20 lines/columns) while
  -- Float value specifies percentage (i.e. 0.3 - 30% of available lines/columns)
  -- Elements are the elements shown in the layout (in order).
  -- Layouts are opened in order so that earlier layouts take priority in window sizing.
  layouts = {
    {
      elements = {
      -- Elements can be strings or table with id and size keys.
        { id = "scopes", size = 0.25 },
        "breakpoints",
        "stacks",
        "watches",
      },
      size = 40, -- 40 columns
      position = "left",
    },
    {
      elements = {
        "repl",
        "console",
      },
      size = 0.25, -- 25% of total lines
      position = "bottom",
    },
  },
  controls = {
    -- Requires Neovim nightly (or 0.8 when released)
    enabled = true,
    -- Display controls in this element
    element = "repl",
    icons = {
      pause = "",
      play = "",
      step_into = "",
      step_over = "",
      step_out = "",
      step_back = "",
      run_last = "↻",
      terminate = "□",
    },
  },
  floating = {
    max_height = nil, -- These can be integers or a float between 0 and 1.
    max_width = nil, -- Floats will be treated as percentage of your screen.
    border = "single", -- Border style. Can be "single", "double" or "rounded"
    mappings = {
      close = { "q", "<Esc>" },
    },
  },
  windows = { indent = 1 },
  render = {
    max_type_length = nil, -- Can be integer or nil.
    max_value_lines = 100, -- Can be integer or nil.
  }
})

-- dap.listeners.after.event_initialized["dapui_config"] = function()
--   dapui.open()
-- end
-- dap.listeners.before.event_terminated["dapui_config"] = function()
--   dapui.close()
-- end
-- dap.listeners.before.event_exited["dapui_config"] = function()
--   dapui.close()
-- end
