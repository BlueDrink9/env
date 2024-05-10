local idemaps = vim.g.IDE_mappings

return {

    {
        "milanglacier/yarepl.nvim",
        opts = function(_, opts)
            return opts
        end
    },


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

	{
		"https://github.com/Vigemus/iron.nvim",
		-- version="*", -- v3 currently broken for latest nvim, need master
		opts = function(_, opts)
			vim.keymap.set("n", idemaps.REPLSendEndLine,
				function()
					vim.cmd[[normal v$]];
					require'iron.core'.visual_send()
			end)
			return {
				config = {
					-- Whether a repl should be discarded or not
					scratch_repl = true,
					repl_definition = {
						sh = {
							command = { "bash" },
						},
					},
					repl_open_cmd = require("iron.view").split.horizontal.botright(0.2),
					-- repl_open_cmd = require("iron.view").split.botright(function
					--  if vim.o.columns > 180 then
					--      return require("iron.view").split.vertical.botright(50)
					--  else
					--      return require("iron.view").split.horizonal.botright(0.20)
					--  end
					-- end
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
					italic = true,
				},
				ignore_blank_lines = false,
			}
		end,
		config = function(_, opts)
			require("iron.core").setup(opts)
		end,
		cmd = { "IronFocus", "IronRepl" },
		keys = {
			{ "<space>s<c-s>", "<cmd>IronFocus<cr>" },
			{ idemaps.REPLSend, mode = { "n", "v" } },
			{ idemaps.REPLSendFile },
			{ idemaps.REPLSendEndLine },
			{ idemaps.REPLSendLine },
		},
	},

}
