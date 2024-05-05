local maps = vim.g.IDE_mappings

local specs = {
   -- {import = "lazyvim.plugins.extras.dap.core", cond = IsPluginUsed("LazyVim")},
   -- {import = "lazyvim.plugins.extras.dap.nlua", cond = IsPluginUsed("LazyVim")},

   {
      'mfussenegger/nvim-dap',
      -- Stop this being loaded by lspconfig somewhere.
      -- Only want this module loaded on a debug command.
      module=false,
      keys = {maps.debugFile},
      cmd = {
         "Debug",
         "DebugTestClass",
         "DebugTestMethod",
      },

      opts = function(_, opts)
         -- See `:help vim.lsp.*` for documentation on any of the below functions
         local dap_nbufmaps = {
            [maps.debugFile] = 'continue()',
            [maps.debugReset] = 'terminate()',
            [maps.debugContinue] = 'continue()',
            [maps.debugRestart] = 'run_last()',
            [maps.debugPause] = 'pause()',
            [maps.debugStepOver] = 'step_over()',
            [maps.debugStepInto] = 'step_into()',
            [maps.debugStepOut] = 'step_out()',
            [maps.debugStepBack] = 'step_back()',
            [maps.debugRunToHere] = 'run_to_cursor()',
            [maps.debugInspect] = 'repl.toggle()',
            [maps.debugFrameUp] = 'up()',
            [maps.debugFrameDown] = 'down()',
            [maps.debugShowStoppedLine] = 'focus_frame()',
            [maps.debugShowStoppedLine2] = 'focus_frame()',
            [maps.setBreakpoint] = 'toggle_breakpoint()',
            [maps.setBreakpointConditional] = "set_breakpoint(vim.fn.input('Breakpoint condition: '))",
            [maps.breakpointList] = 'list_breakpoints()',
            -- [debugShowOutput] = '',
            -- [debugHover] = '',
         }

         local dapui_nbufmaps = {
            -- [maps.REPLSend]  = 'eval()',
            -- require("dapui").toggle()
            ["<leader>de"]  = 'eval()',
            ["<leader>du"] = 'toggle({})',
         }
         local dapui_vbufmaps = {
            [maps.REPLSend]  = 'eval()',
            -- require("dapui").toggle()
         }

         local dap_breakpoints_bufmaps = {
            [maps.breakpointNext] = 'next()',
            [maps.breakpointPrev] = 'prev()',
         }

         local map_table_prefixes = {
            ["require'dap'."]= dap_nbufmaps,
            ["require'dapui'."]= dapui_nbufmaps,
            ["require'goto-breakpoints'."]= dap_breakpoints_bufmaps,
         }

         -- Ensures mappings are only set for filetypes with installed DAPs
         Set_up_dap_buffer = function()
            -- When jumping to a fileline from current tab, don't change tabs if you
            -- dont' have to.
            vim.opt_local.switchbuf="useopen,uselast"
            vim.cmd[[command! -buffer Debug lua require("dapui").toggle()]]

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

         Set_up_mason_dap_autocmd = function(source_name)
            vim.api.nvim_create_autocmd("Filetype", {
               group = "myIDE",
               pattern = source_name,
               callback = Set_up_dap_buffer,
            })
         end

         -- Au to get mappings in dapui buffers as well.
         Set_up_mason_dap_autocmd("dapui_*")

         DapHandlers = {
            function(config)
               for _, ft in ipairs(config.filetypes) do
                  Set_up_mason_dap_autocmd(ft)
               end
               require'mason-nvim-dap'.default_setup(config)
               -- dap.listeners.after.event_initialized["dap"] = on_attach(vim.api.nvim_buf_get_number)
            end,
            python = function(config)
               Set_up_mason_dap_autocmd("python")
               require('dap-python').setup(vim.g.python3_host_prog)
               vim.cmd[[command! -buffer DebugTestMethod lua require('dap-python').test_method()]]
               vim.cmd[[command! -buffer DebugTestClass lua require('dap-python').test_class()]]

               require'dap'.adapters.python = {
                  type = "executable",
                  -- command = "/usr/bin/python3",
                  command = "python3",
                  args = {
                     "-m",
                     "debugpy.adapter",
                  },
               }

               -- Insert because dap-python already configures it
               table.insert(require'dap'.configurations.python, {
                  {
                     type = "python",
                     request = "launch",
                     name = "Launch file",
                     program = "${file}", -- This configuration will launch the current file if used.
                  },
               })
               require'mason-nvim-dap'.default_setup(config)
            end
         }

         -- If dap loads after mason-nvim-dap, need to re-initialise
         -- handlers for mason-nvim-dap
         if MasonDapOpts then
            MasonDapOpts.handlers = DapHandlers
            require("mason-nvim-dap").setup(MasonDapOpts)
         end

         -- Check whether the file that triggered this load should be set
         -- up.
         vim.cmd.doautocmd("FileType")

      end,

      config = function(_, opts)
         vim.api.nvim_set_hl(0, "DapStoppedLine", { default = true, link = "Visual" })
         -- require'persistent-breakpoints'
         -- require("telescope").load_extension('dap')
         require'nvim-dap-virtual-text'
         require'goto-breakpoints'
      end,

      dependencies = {

         {'jayp0521/mason-nvim-dap.nvim',
            dependencies={'williamboman/mason.nvim', 'mfussenegger/nvim-dap'},
            cmd = { "DapInstall", "DapUninstall" },
            opts = {
               -- ensure_installed = {'stylua', 'jq'}
               automatic_installation = false,
               automatic_setup = true,
            },
            config = function(_, opts)
               -- If this loads after nvim-dap (i.e. this was requested to
               -- load first, and nvim-dap was loaded as its dependency),
               -- then set up here.
               -- Otherwise, if dap was requested first and loaded this
               -- beforehand as a dependency, set it up as part of dap.
               MasonDapOpts = opts
               if DapHandlers then
                  opts.handlers = DapHandlers
                  require("mason-nvim-dap").setup(opts)
               end
            end
         },

         {'rcarriga/nvim-dap-ui',
            lazy=true,
            opts = {
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
            },
            dependencies='nvim-neotest/nvim-nio',
         },

         -- This might need remapping to use its own toggle breakpoint :(
         {'https://github.com/Weissle/persistent-breakpoints.nvim',
            opts = {load_breakpoints_event = { "FileReadPost" }},
         },

         {'https://github.com/ofirgall/goto-breakpoints.nvim', lazy=true},

         {'theHamsta/nvim-dap-virtual-text',
            dependencies='nvim-treesitter/nvim-treesitter',
            opts={commented=true},
         },

         {'mfussenegger/nvim-dap-python',
            dependencies='nvim-treesitter/nvim-treesitter',
         },

         {'https://github.com/nvim-telescope/telescope-dap.nvim',
            config = function() require("telescope").load_extension('dap') end,
         },

         { 
            'rcarriga/cmp-dap',
            dependencies = {
               {
                  'hrsh7th/nvim-cmp',
                  opts = function(_, opts)
                     local old_enabled = opts.enabled
                     opts.enabled = function()
                        return old_enabled() and (
                           vim.api.nvim_buf_get_option(0, "buftype") ~= "prompt"
                           or require("cmp_dap").is_dap_buffer()
                        )
                     end
                     require("cmp").setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, {
                        sources = {
                           { name = "dap" },
                        },
                     })
                     return opts
                  end
               }
            }
         }

      }
   }
}


         return specs
