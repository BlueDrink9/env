if not IsPluginUsed("LazyVim") then
	return {}
end

-- surround mappings
-- replace
-- exchange
-- arg swap
-- persistance/session management replace
-- mason - delay installs/don't ensure installed
-- space c replacement/remap
-- Rg
-- Fugitive + neogit
-- Windowtitle
-- cmp: don't select first one automatically
-- Startup

return {

	{ "neovim/nvim-lspconfig", cond = vim.g.ideMode == 1 },
	{ "hrsh7th/nvim-cmp", cond = vim.g.ideMode == 1 },
	{ "conform.nvim", cond = vim.g.ideMode == 1 },
	{ "indent-blankline.nvim", cond = vim.g.ideMode == 1 },
	{ "nvim-telescope/telescope.nvim", cond = vim.g.ideMode == 1 },

	{
		"folke/noice.nvim",
		enabled = false,
		cond = vim.g.vscode ~= 1,
		-- opts will be merged with the parent spec
		opts = {
			messages = { enabled = false },
			cmdline = { enabled = false },
		},
		config = function()
			vim.cmd("echom noice loaded")
			vim.opt.lazyredraw = false
		end,
		init = function()
			vim.opt.lazyredraw = false
		end,
		event = "VeryLazy",
	},

	{
		"nvim-treesitter/nvim-treesitter",
		cond = vim.g.liteMode == 0,
	},

	{
		"echasnovski/mini.surround",
		enabled = false,
		opts = {
			mappings = {
				add = "ys",
				delete = "ds",
				find = "ysf",
				find_left = "ysF",
				highlight = "ysh",
				replace = "ysr",
				update_n_lines = "ysn",
			},
		},
	},

	{
		"echasnovski/mini.comment",
		enabled = false,
		opts = {
			mappings = {
				comment = "<leader>c",
				comment_line = "<leader>cc",
				textobject = "ac",
			},
		},
	},

	{
		"echasnovski/mini.ai",
		opts = {
			custom_textobjects = {
				-- Replace class key with C so that comment can be c
				C = function()
					return require("mini.ai").config.custom_textobjects.c
				end,
			},
		},
		keys = {
			{ "a", mode = "o" },
			{ "i", mode = "o" },
		},
	},

	{
		"L3MON4D3/LuaSnip",
		cond = vim.g.ideMode == 1,
		opts = function(_, opts)
			local snipdir = vim.g.configDir .. vim.fn.expand("/runtimepath/snippets")
			-- load snippets from path/of/your/nvim/config/my-cool-snippets
			require("luasnip.loaders.from_snipmate").lazy_load({
				paths = { snipdir },
			})
			require("luasnip.loaders.from_vscode").lazy_load({
				paths = { snipdir },
			})
		end,
	},

	{
		"nvimtools/none-ls.nvim",
		cond = vim.g.ideMode == 1,
		opts = function()
			local nls = require("null-ls")
			return {
				sources = {
					nls.builtins.formatting.stylua,
					nls.builtins.formatting.shfmt,
					-- nls.builtins.diagnostics.flake8,
				},
			}
		end,
	},

	{
		"folke/which-key.nvim",
		cond = vim.g.vscode ~= 1,
		opts = {
			plugins = { spelling = true },
			defaults = {
				mode = { "x", "v" },
				-- Remove these because they begin operator funcs, which get overritten
				-- by which-key here if lazy-loaded. (E.g. comment)
				["<leader>c"] = nil,
				["<leader>s"] = nil,
				["<leader>u"] = nil,
			},
			triggers_blacklist = {
				v = { "<leader>c" },
			},
		},
	},

	{
		"nvim-ts-autotag",
		ft = {
			"astro",
			"glimmer",
			"handlebars",
			"html",
			"javascript",
			"jsx",
			"markdown",
			"php",
			"rescript",
			"svelte",
			"tsx",
			"typescript",
			"vue",
			"xml",
		},
		cond = vim.g.ideMode == 1,
	},
}
