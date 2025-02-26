return {
	{ "dstein64/vim-startuptime" },

	{ import = "lazyvim.plugins.extras.editor.dial", cond = IsPluginUsed("LazyVim") },

	{ import = "plugins.treesitter" },
	{ import = "plugins.firenvim" },
	{ import = "plugins.vscode-neovim" },
	{ import = "plugins.lualine" },
	{ import = "plugins.repl" },

	{ "andymass/vim-matchup", event = "VeryLazy" },

	{
		"echasnovski/mini.align",
		opts = {
			mappings = { start = "", start_with_preview = "gA" },
		},
		cmd = "Align",
		keys = { {"gA", mode={"n", "x"}, desc="Align"} },
		init = function()
			vim.api.nvim_create_user_command("Align", function()
				require("mini.align").action_visual(false)
			end, {})
		end,
	},

	{ "https://github.com/NMAC427/guess-indent.nvim", event = "VeryLazy", config = true },

	{ "https://github.com/stevearc/dressing.nvim.git", event = "VeryLazy" },

	{
		"ggandor/leap.nvim",
		config = function()
			-- Will not override existing mappings.
			require("leap").add_default_mappings()
		end,
		opts = {
			prev_target = { "<tab>" }, -- Remove ',' from the default
		},
		keys = { "s", "S" },
		-- Defines text objects like `riw`, `raw`, etc., instead of
		-- targets.vim-style `irw`, `arw`. (Note: prefix is forced if a custom
		-- text object does not start with "a" or "i".)
		prefix = true,
		-- The yanked text will automatically be pasted at the cursor position
		-- if the unnamed register is in use.
		paste_on_remote_yank = true,
		-- Can't really figure out how to use this one atm.
		-- if vim.fn.IsPluginUsed('leap-ast.nvim') == 1 and vim.fn.IsPluginUsed('nvim-treesitter') == 1 then
		--     vim.keymap.set(modes, ',W',
		--     function() require'leap-ast'.leap() end,
		--     {desc='leap to ast elemenjt'})
		-- end
		-- Adds very little but needed for lazyvim to not throw a hissy
		dependencies = { import = "lazyvim.plugins.extras.editor.leap", cond=IsPluginUsed("LazyVim")},

	},
	{
		"https://github.com/ggandor/leap-spooky.nvim",
		config = true,
		keys = function()
			local out = {}
			for _, key1 in ipairs({ "i", "a" }) do
				for _, key2 in ipairs({ "r", "R", "m", "M" }) do
					table.insert(out, { key1 .. key2, mode = "o" })
				end
			end
			return out
		end,
	},
	-- {'https://github.com/ggandor/leap-ast.nvim'},

	{
		"https://github.com/Nguyen-Hoang-Nam/nvim-preview-csv",
		-- Want to keep just in case we want the movement as well as the view
		init = function()
			vim.g.csv_autocmd_arrange = 0
		end,
		opts = {
			max_csv_line = 100,
		},
		ft = "csv",
	},

	{
		"https://github.com/lewis6991/gitsigns.nvim.git",
		event = "VeryLazy",
		on_attach = function(buffer)
			local gs = package.loaded.gitsigns
			local function map(mode, l, r, desc)
				vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
			end
			-- stylua: ignore start
			map("n", "]h", gs.next_hunk, "Next Hunk")
			map("n", "[h", gs.prev_hunk, "Prev Hunk")
			map({ "n", "v" }, "<leader>gs", ":Gitsigns stage_hunk<CR>", "Stage Hunk")
			map({ "n", "v" }, "<leader>ghr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
			map("n", "<leader>gS", gs.stage_buffer, "Stage Buffer")
			map("n", "<leader>gu", gs.undo_stage_hunk, "Undo Stage Hunk")
			map("n", "<leader>gR", gs.reset_buffer, "Reset Buffer")
			map("n", "<leader>gp", gs.preview_hunk, "Preview Hunk")
			map("n", "<leader>ghb", function() gs.blame_line({ full = true }) end, "Blame Line")
			map("n", "<leader>ghd", gs.diffthis, "Diff This")
			map("n", "<leader>ghD", function() gs.diffthis("~") end, "Diff This ~")
			map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
		end,
		opts = {
			-- signs = {
			--   add = {hl = 'GitSignsAdd', text = '+', numhl='GitSignsAddNr', linehl='GitSignsAddLn'},
			-- },
			signcolumn = true,
			numhl = true,
		},
	},

	-- Extra filetypes

	{
		"https://github.com/salkin-mada/openscad.nvim",
		ft = "scad",
	},

	{
		"https://github.com/nvim-orgmode/orgmode",
		ft = "org",
	},

	{
		"https://github.com/chomosuke/typst-preview.nvim",
		ft = "typst",
		opts=function(_, opts)
			require('typst-preview').dependencies_bin = 'tinymist'	
		end
	},

	{
		-- Has come up with a decent set of low-clash bindings I think.
		"julienvincent/nvim-paredit",
		ft = {
			-- Currently only supports closure :/
			-- 'commonlisp',
			-- 'elisp',
			"closure",
		},
		opts = {
			indent = {
				enabled = true,
			},
		},
	},

	{
		"dundalek/parpar.nvim",
		dependencies = { "gpanders/nvim-parinfer", "julienvincent/nvim-paredit" },
		opts = {},
		ft = {
			"commonlisp",
			"elisp",
			"fennel",
			"scheme",
			"closure",
		},
	},
	{
		-- Has come up with a decent set of low-clash bindings I think.
		-- >) >( <( <) to slurp/barf next/prev
		"julienvincent/nvim-paredit",
		ft = {
			-- Currently only supports closure natively. Extend
			-- with plugins
			-- 'commonlisp',
			-- 'elisp',
			"closure",
		},
		opts = {
			indent = {
				enabled = true,
			},
		},
	},
	{
		"julienvincent/nvim-paredit-fennel",
		dependencies = { "julienvincent/nvim-paredit" },
		ft = { "fennel" },
		config = function()
			require("nvim-paredit-fennel").setup()
		end,
	},
	{
		"ekaitz-zarraga/nvim-paredit-scheme",
		dependencies = { "julienvincent/nvim-paredit" },
		ft = { "scheme" },
		config = function()
			require("nvim-paredit-scheme").setup(require("nvim-paredit"))
		end,
	},


----


	{
		"chrishrb/gx.nvim",
		keys = { { "gx", "<cmd>Browse<cr>", mode = { "n", "x" } } },
		cmd = { "Browse" },
		init = function ()
			vim.g.netrw_nogx = 1 -- disable netrw gx
		end,
		dependencies = { "nvim-lua/plenary.nvim" },
		submodules = false, -- not needed, submodules are required only for tests
		opts = {
			handler_options = {
				search_engine = "duckduckgo", -- you can select between google, bing, duckduckgo, and ecosia
			},
		},
	},

	{
		-- Mainly taken from lazyvim, but leaving out any IDE and lazy bits.
		-- Lazyvim can add them back in when in IDE mode.
		"akinsho/bufferline.nvim",
		event = "VeryLazy",
		keys = {
			{ "<leader>bp", "<Cmd>BufferLineTogglePin<CR>", desc = "Toggle pin" },
			{ "<leader>bP", "<Cmd>BufferLineGroupClose ungrouped<CR>", desc = "Delete non-pinned buffers" },
		},
		opts = function(_, opts)
			opts.options = opts.options or {}
			opts.options.numbers = "buffer_id"
			opts.options.close_command = function(n)
				require("mini.bufremove").delete(n, false)
			end
			opts.options.right_mouse_command = function(n)
				require("mini.bufremove").delete(n, false)
			end
			opts.options.always_show_bufferline = false
		end,
	},

	{
		"tzachar/highlight-undo.nvim",
		config = true,
		keys = { "u", "<C-r>" },
	},

	{
		"sindrets/diffview.nvim",
		config = true,
		cmd = {
			"DiffviewFileHistory",
			"DiffviewOpen",
		},
		event = "OptionSet diff",
		lazy = not vim.o.diff,
	},

	{ "kwkarlwang/bufresize.nvim", event = "VeryLazy" },

	{
		"bkad/camelcasemotion",
		keys = function()
			local out = {}
			for _, key in ipairs({ "w", "b", "e", "ge" }) do
				table.insert(out, { "-" .. key, "<Plug>CamelCaseMotion_" .. key, mode = { "n", "v", "o" } })
			end
			for _, key in ipairs({ "iw", "aw" }) do
				table.insert(out, {
					"-" .. key,
					"<Plug>CamelCaseMotion_" .. key,
					mode = { "v", "o" },
				})
			end
			return out
		end,
	},

	{
		"hachy/cmdpalette.nvim",
		keys = {
			{ "q;", "<Cmd>Cmdpalette<CR>", mode = { "n", "x" } },
			{ "kv", "<Cmd>Cmdpalette<CR><CR>", mode = { "c" } },
			{ "vk", "<Cmd>Cmdpalette<CR><CR>", mode = { "c" } },
		},
		opts = {
			win = {
				height = 0.1,
				width = 0.8,
				border = "rounded",
				-- Title requires nvim-0.9 or higher.
				title = "Cmdpalette",
				title_pos = "center",
			},
			sign = {
				text = ":",
			},
			buf = {
				filetype = "cmdpalette",
				syntax = "vim",
			},
			delete_confirm = true,
			show_title = false,
		},
		config = function(_, opts)
			require("cmdpalette").setup(opts)
			-- vim.keymap.set({ "n" }, ";", "<Cmd>Cmdpalette<CR>")
			-- vim.keymap.set({ "x" }, ";", "<Cmd>Cmdpalette<CR>'<,'>")
			-- vim.opt_local.completeopt:remove("noselect")
			local function bufmap()
				-- Need the <c-n> to select the first option for some reason
				vim.keymap.set(
					"i",
					"<tab>",
					"<c-x><c-v>",
					-- 'pumvisible() ? "<c-n>" : "<c-x><c-v><c-n>"',
					{ buffer = true }
				)
				vim.keymap.set({ "n", "v" }, "<CR>", require("cmdpalette").execute_cmd, { buffer = true })
			end

			local function copy_cabbrevs()
				--- Copy cabbrevs into local iabbrevs, for same behaviour as
				--- cmd-mode.
				local result = vim.api.nvim_exec2("cabbrev", { output = true }).output
				for line in result:gmatch("[^\r\n]+") do
					local mode, abbrev, expansion = line:match("^%s*(%S+)%s+(%S+)%s+%*?%s*(.+)$")
					-- Doens't look like expr abbrevs are marked in any way. But
					-- most of the ones we care about will be returning strings in
					-- some way, so will usually have quotes in them. Hopefully a
					-- decent enough heuristic...
					if expansion:find('"') or expansion:find("'") then
						vim.cmd.iabbr({ "<expr>", "<buffer>", abbrev, expansion })
					else
						vim.cmd.iabbr({ "<buffer>", abbrev, expansion })
					end
				end
			end

			vim.api.nvim_create_autocmd("filetype", {
				pattern = "cmdpalette",
				group = "myPlugins",
				callback = function()
					bufmap()
					copy_cabbrevs()
				end,
			})
		end,
	},

	-- {
	-- 	"https://github.com/akinsho/toggleterm.nvim",
	-- 	keys = "<C-s>",
	-- 	opts = function()
	-- 		local send = require("toggleterm").send_lines_to_terminal
	-- 		vim.keymap.set("v", "<space>s", function()
	-- 			send("visual_selection", false, { args = vim.v.count })
	-- 		end)
	-- 		local set_opfunc = vim.fn[vim.api.nvim_exec(
	-- 			[[
 --                func s:set_opfunc(val)
 --                let &opfunc = a:val
 --                endfunc
 --                echon get(function('s:set_opfunc'), 'name')
 --                    ]],
	-- 			true
	-- 		)]
	-- 		vim.keymap.set("n", "<leader>s", function()
	-- 			set_opfunc(function(motion_type)
	-- 				send(motion_type, false, { args = vim.v.count })
	-- 			end)
	-- 			vim.api.nvim_feedkeys("g@", "n", false)
	-- 		end)
	-- 		vim.keymap.set("n", "<leader>ss", function()
	-- 			set_opfunc(function(motion_type)
	-- 				send(motion_type, false, { args = vim.v.count })
	-- 			end)
	-- 			vim.api.nvim_feedkeys("g@_", "n", false)
	-- 		end)
	-- 		vim.keymap.set("n", "<leader>S", function()
	-- 			set_opfunc(function(motion_type)
	-- 				send(motion_type, false, { args = vim.v.count })
	-- 			end)
	-- 			vim.api.nvim_feedkeys("ggg@G''", "n", false)
	-- 		end)
	-- 		opts = {
	-- 			open_mapping = [[<c-s>]],
	-- 			direction = "horizontal",
	-- 			start_in_insert = false,
	-- 			insert_mappings = false,
	-- 			terminal_mappings = true,
	-- 			persist_mode = true,
	-- 			close_on_exit = false, -- Otherwise may miss startup errors
	-- 			auto_scroll = false,
	-- 			shell = vim.o.shell,
	-- 		}
	-- 		if vim.fn.has("win32") == 1 then
	-- 			if vim.fn.Executable("pwsh") == 1 then
	-- 				opts.shell = "pwsh"
	-- 			else
	-- 				opts.shell = "powershell.exe"
	-- 			end
	-- 		end
	-- 		return opts
	-- 	end,
	-- },

	{
		-- Jump to previous buffer in jumplist (like c-o but only for buffer changes)
		"kwkarlwang/bufjump.nvim",
		keys = {
			{"<leader>}", function() require('bufjump').forward() end, {desc="Jump to next buffer in jumplist"} },
			{"<leader>{", function() require('bufjump').backward() end, {desc="Jump to previous buffer in jumplist"} },
		},
		opts = {
			forward_key = false,
			backward_key = false,
		}
	},

	{
		-- Similar to bufjump but with explicit list of 'marks'
		"ThePrimeagen/harpoon",
		keys = {
			{"m<space>", function() require('harpoon'):list():add() end},
			{"<leader>]", function() require('harpoon'):list():next() end, {desc="Jump to next harpoon"} },
			{"<leader>[", function() require('harpoon'):list():prev() end, {desc="Jump to previous harpoon"} },
		},
		branch = "harpoon2",
		dependencies = { "nvim-lua/plenary.nvim" },
		config = true,
	},

	{
		"MagicDuck/grug-far.nvim",
		opts = { headerMaxWidth = 80 },
		cmd = "GrugFar",
		keys = {
			{ "<leader>?r", function() require("grug-far").grug_far() end, mode = { "n", "v" }, desc = "Search and Replace (Far)"},
			{ "<leader>?/", function() require("grug-far").grug_far() end, mode = { "n", "v" }, desc = "Search and Replace (Far)"},
		},
	},

	-- Handled by lazyvim
	-- {
	-- 	'monaqa/dial.nvim',
	-- 	keys = {
	-- 		{ "<C-a>", function() return M.dial(true) end, expr = true, desc = "Increment", mode = {"n", "v"} },
	-- 		{ "<C-x>", function() return M.dial(false) end, expr = true, desc = "Decrement", mode = {"n", "v"} },
	-- 		{ "g<C-a>", function() return M.dial(true, true) end, expr = true, desc = "Increment", mode = {"n", "v"} },
	-- 		{ "g<C-x>", function() return M.dial(false, true) end, expr = true, desc = "Decrement", mode = {"n", "v"} },
	-- 	},
	-- },

	-- {
	-- 	"willothy/flatten.nvim",
	-- 	opts = {
	-- 		window = "alternate"
	-- 	},
	-- 	init = function()
	-- 		-- Nb: this isn't working because the shell rc
	-- 		-- overwrites these variables. Need to come up with an
	-- 		-- alternative solution.
	-- 		for _, v in ipairs({"EDITOR", "VISUAL", "SUDO_EDITOR",
	-- 			"FCEDIT", "GIT_EDITOR"}) do
	-- 			vim.fn.setenv(v, 'nvim --cmd "let g:flatten_wait=1"')
	-- 		end
	-- 	end,
	-- 	-- Ensure that it runs first to minimize delay when opening file from terminal
	-- 	-- Won't work if lazy-loaded later :(
	-- 	lazy = false,
	-- 	priority = 1001,
	-- 	-- event="TermOpen",
	-- },

	{
		"lambdalisue/vim-suda",
		cmd = {"SudaRead", "SudaWrite"},
		init = function()
			vim.cmd[[
			command! -bang -nargs=* SudoSave SudaWrite
			cmap W! SudaWrite
			]]
		end
	},

	{
		"hands-free-vim/cursorless.nvim",
		config = true,
		enabled = vim.g.loaded_node_provider and vim.g.loaded_node_provider > 0
	},
	{ "hands-free-vim/talon.nvim", config = true, },

}
