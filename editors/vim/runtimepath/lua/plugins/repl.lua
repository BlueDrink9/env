local idemaps = vim.g.IDE_mappings

return {

  --   {
  --       "milanglacier/yarepl.nvim",
  --       opts = {
		-- 	buflisted=false,
		-- },
		-- keys = {
		-- 	-- { "<leader>rt", "<cmd>ReplToggle<cr>", desc = "Toggle nvim-repl" },
		-- 	-- { "<leader>rc", "<cmd>ReplRunCell<cr>", desc = "nvim-repl run cell" },
		--   },
  --   },

	{
		"kassio/neoterm",
		cond = vim.g.vscode ~= 1,
		init=function()
			-- Man, I really don't love the neoterm defaults huh
			vim.g.neoterm_default_mod = "botright"
			vim.g.neoterm_size = 10
			-- vim.g.neoterm_direct_open_repl = 1
			vim.g.neoterm_term_per_tab = 1
			vim.g.neoterm_autoinsert = 1
			-- not actually sure what this does
			vim.g.neoterm_repl_same_shell = 1
			-- vim.g.neoterm_repl_enable_ipython_paste_magic = 1
			if vim.fn.has("win32") then
				vim.g.neoterm_eof = "\r"
			end
			-- unsure whether I always ant this behaviour
			vim.g.neoterm_autoscroll = 1
			vim.g.neoterm_fixedsize = 1
			-- vim.g.neoterm_auto_repl_cmd = 1
			-- vim.g.neoterm_command_prefix = "\\\\edda"  -- vi-binding clear line and enter insert
			-- not ideal for pure python, but I'm assuming i'll be using
			-- ipython all the time anyway, ideally.
			vim.g.neoterm_bracketed_paste = 1
		end,
		config = function(_, opts)  -- Doesn't have a setup func
			-- neoterm window size only when using it in horizontal mode
			vim.cmd[[
				let g:neoterm_callbacks = {}
				function! g:neoterm_callbacks.before_create_window(instance)
					if a:instance.mod =~ 'vert'
						let g:neoterm_size = ''
					else
						let g:neoterm_size = 10
					end
				endfunction
			]]
			vim.api.nvim_create_user_command("ReplChange", "TREPLSetTerm", {nargs=1})
			-- See #286	
			-- vim.api.nvim_create_user_command("TRegisterAsNeoterm", "call neoterm#new({'from_event': 1})", {nargs=1})
			-- On load, run ftdetect again
			vim.cmd[[filetype detect]]
		end,

		keys = {
			{idemaps.TermToggle, '<cmd>Ttoggle ' .. vim.o.shell .. ' ' .. vim.fn.substitute(vim.o.shellcmdflag, '[-/]c', '', '') .. '<cr>', mode={"n", "v", "i", "t"}},
			{idemaps.REPLSendLine, '<Plug>(neoterm-repl-send-line)'},
			{idemaps.REPLSend, '<Plug>(neoterm-repl-send)', mode={"n", "v"}},
			{idemaps.REPLToggle, '<cmd>Ttoggle<cr>'},
			{idemaps.REPLCancel, '<cmd>Texec <cr>'},
			{idemaps.REPLClear, '<cmd>Tclear<cr>'},
			{idemaps.REPLClose, '<cmd>Tclose!<cr>'},
			{idemaps.REPLSendFile, '<cmd>TREPLSendFile<cr>'},
			{idemaps.REPLSendFile2, '<cmd>TREPLSendFile<cr>'},
		},
	},

	-- {
	-- 	"Olical/conjure",
	-- 	ft = { "clojure", "fennel", "python" }, -- etc
	-- 	-- [Optional] cmp-conjure for cmp
	-- 	dependencies = {
	-- 		{
	-- 			"PaterJason/cmp-conjure",
	-- 			config = function()
	-- 				local cmp = require("cmp")
	-- 				local config = cmp.get_config()
	-- 				table.insert(config.sources, {
	-- 					name = "buffer",
	-- 					option = {
	-- 						sources = {
	-- 							{ name = "conjure" },
	-- 						},
	-- 					},
	-- 				})
	-- 				cmp.setup(config)
	-- 			end,
	-- 		},
	-- 	},
	-- 	config = function(_, opts)
	-- 		require("conjure.main").main()
	-- 		require("conjure.mapping")["on-filetype"]()
	-- 	end,
	-- 	init = function()
	-- 		-- Set configuration options here
	-- 		vim.g["conjure#debug"] = true
	-- 	end,
	-- },



	-- Doesn't work on Windows
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

	-- {
	-- 	"https://github.com/Vigemus/iron.nvim",
	-- 	-- version="*", -- v3 currently broken for latest nvim, need master
	-- 	opts = function(_, opts)
	-- 		vim.keymap.set("n", idemaps.REPLSendEndLine,
	-- 			function()
	-- 				vim.cmd[[normal v$]];
	-- 				require'iron.core'.visual_send()
	-- 		end)
	-- 		return {
	-- 			config = {
	-- 				-- Whether a repl should be discarded or not
	-- 				scratch_repl = true,
	-- 				-- repl_definition = {
	-- 				-- 	sh = {
	-- 				-- 		command = { "bash" },
	-- 				-- 	},
	-- 				-- },
	-- 				repl_open_cmd = require("iron.view").split.horizontal.botright(0.2),
	-- 				-- repl_open_cmd = require("iron.view").split.botright(function
	-- 				--  if vim.o.columns > 180 then
	-- 				--      return require("iron.view").split.vertical.botright(50)
	-- 				--  else
	-- 				--      return require("iron.view").split.horizonal.botright(0.20)
	-- 				--  end
	-- 				-- end
	-- 			},
	-- 			keymaps = {
	-- 				send_motion = idemaps.REPLSend,
	-- 				visual_send = idemaps.REPLSend,
	-- 				send_file = idemaps.REPLSendFile,
	-- 				send_line = idemaps.REPLSendLine,
	-- 				interrupt = idemaps.REPLCancel,
	-- 				exit = idemaps.REPLClose,
	-- 				clear = idemaps.REPLClear,
	-- 			},
	-- 			highlight = {
	-- 				italic = true,
	-- 			},
	-- 			ignore_blank_lines = false,
	-- 		}
	-- 	end,
	-- 	config = function(_, opts)
	-- 		require("iron.core").setup(opts)
	-- 	end,
	-- 	cmd = { "IronFocus", "IronRepl" },
	-- 	keys = {
	-- 		{ "<space>s<c-s>", "<cmd>IronFocus<cr>" },
	-- 		{ idemaps.REPLSend, mode = { "n", "v" } },
	-- 		{ idemaps.REPLSendFile },
	-- 		{ idemaps.REPLSendEndLine },
	-- 		{ idemaps.REPLSendLine },
	-- 	},
	-- },

}
