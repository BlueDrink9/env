require("mason").setup()
local dap = require("dap")
local mason_dap = require("mason-nvim-dap")
mason_dap.setup()
local dapui = require("dapui")

require("nvim-dap-virtual-text").setup {
   commented = true,
}

-- This might need remapping to use its own toggle breakpoint :(
require('persistent-breakpoints').setup{
   load_breakpoints_event = { "FileReadPost" }
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
   [maps.debugShowStoppedLine] = 'focus_frame()',
   [maps.setBreakpoint] = 'toggle_breakpoint()',
   [maps.setBreakpointConditional] = "set_breakpoint(vim.fn.input('Breakpoint condition: '))",
   [maps.breakpointList] = 'list_breakpoints()',
   -- [debugShowOutput] = '',
   -- [debugHover] = '',
}

local dapui_nbufmaps = {
   -- [maps.REPLSend]  = 'eval()',
-- require("dapui").toggle()
}
local dapui_vbufmaps = {
   [maps.REPLSend]  = 'eval()',
-- require("dapui").toggle()
}

local dap_breakpoints_bufmaps = {
   [maps.breakpointNext] = 'next()',
   [maps.breakpointPrev] = 'prev()',
}

map_table_prefixes = {
   ["require'dap'."]= dap_nbufmaps,
   ["require'dapui'."]= dapui_nbufmaps,
   ["require'goto-breakpoints'."]= dap_breakpoints_bufmaps,
}



mason_dap.setup({
      -- ensure_installed = {'stylua', 'jq'}
   })

-- Ensures mappings are only set for filetypes with installed DAPs
local set_up_buffer = function()
   -- When jumping to a fileline from current tab, don't change tabs if you
   -- dont' have to.
   vim.opt_local.switchbuf="useopen,uselast"
   vim.cmd[[command! -buffer Debug lua require("dapui").toggle()]]
   --
   -- Mappings.
   local bufnr = vim.api.nvim_buf_get_number(0)
   for prefix, table in pairs(map_table_prefixes) do
      require('my/utils').map_table_with_prefix(
         table, "<cmd>lua " .. prefix, "n", {buffer=bufnr}
      )
   end
   for prefix, table in pairs({["require'dapui'"]=dapui_vbufmaps}) do
      require('my/utils').map_table_with_prefix(
         table, "<cmd>lua " .. prefix, "v", {buffer=bufnr}
      )
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
  layouts = {
    {
      position = "right",
      elements = {
        { id = "scopes", size = 0.4 },
        { id = "watches", size = 0.3 },
        { id = "repl", size = 0.3 },
      },
      size = 0.30,
    },
    {
      position = "left",
      elements = {
        "breakpoints",
        "stacks",
      },
      size = 0.15,
    },
    {
      position = "bottom",
      elements = {
        "console",
      },
      size = 0.15,
    },
  },
  controls = {
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

vim.api.nvim_create_autocmd("Filetype", {
   group = "myIDE",
   pattern = "dapui_*",
   callback = set_up_buffer,
})

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
  -- vim.api.nvim_buf_set_keymap(bufnr, 'v', maps.REPLSend, "<Cmd>lua require("dapui").eval()<CR>",
  --   { noremap=true, silent=true })
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close()
end
