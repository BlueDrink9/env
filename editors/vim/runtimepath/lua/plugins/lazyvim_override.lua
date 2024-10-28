if not IsPluginUsed("LazyVim") then
	return {}
end

local spec = {

	{
		"neovim/nvim-lspconfig",
		cond = vim.g.ideMode == 1,
		opts = function()
			local keys = require("lazyvim.plugins.lsp.keymaps").get()
			-- Clear default lsp keymaps
			-- for i=0, #keys do keys[i]=nil end
			keys[#keys + 1] = { "<leader>ca", false, mode={"v", "n"} }
			keys[#keys + 1] = { "<leader>cc", false, mode={"v", "n"} }
			keys[#keys + 1] = { "<leader>cs", false, mode={"v", "n"} }
			keys[#keys + 1] = { "<c-k>", false, mode={"i"} }
		end,
	},

	{ "hrsh7th/nvim-cmp", cond = vim.g.ideMode == 1 },
	{
		"conform.nvim",
		cond = vim.g.ideMode == 1,
		keys = { { "<leader>cF", false, mode = { "v", "n" } } },
	},
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
		opts = function(_, opts)
			opts.ensure_installed = {}
		end
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

	{ "L3MON4D3/LuaSnip", cond = vim.g.ideMode == 1, },

	-- {
	-- 	"nvimtools/none-ls.nvim",
	-- 	cond = vim.g.ideMode == 1,
	-- 	opts = function()
	-- 		local nls = require("null-ls")
	-- 		return {
	-- 			sources = {
	-- 				nls.builtins.formatting.stylua,
	-- 				nls.builtins.formatting.shfmt,
	-- 				-- nls.builtins.diagnostics.flake8,
	-- 			},
	-- 		}
	-- 	end,
	-- },

	{
		"folke/which-key.nvim",
		cond = vim.g.ideMode == 1 and vim.g.vscode ~= 1,
		opts = {
			plugins = { spelling = true },
			spec = {
				{
					mode = { "x", "v" },
					-- Remove these because they begin operator funcs, which get overritten
					-- by which-key here if lazy-loaded. (E.g. comment)
					{"<leader>c",  nil},
					{"<leader>s",  nil},
					-- {"<leader>u",  nil},
				}
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

	{
		'nvim-lualine/lualine.nvim',
		enabled=vim.g.liteMode == 0,
		cond = vim.g.vscode ~= 1,
	},

	{
		"folke/todo-comments.nvim",
		enabled=vim.g.ideMode == 0,
		keys = {
			{ "<leader>st", false },
			{ "<leader>sT", false },
		},
	},

	{
		"CopilotC-Nvim/CopilotChat.nvim",
		cond = vim.g.ideMode == 1,
		opts = function(_, opts)
			-- Want to keep plugin default, will be automatically updated
			-- to newer models faster.
			opts.model = nil
			opts.answer_header = "GPT "
			return opts
		end,
		keys = {
			{ "<c-l>", false },
			{ "<c-s>", "<CR>", ft = "copilot-chat", desc = "Submit Prompt", remap = true },
			{ "<leader>a", false },
			{ "<leader>aa", false },
			{ "<leader>ax", false },
			{ "<leader>aq", false },
			{ "<leader>ad", false },
			{ "<leader>ap", false },
		},
	},

	{
		"danymat/neogen",
		keys = {
			{"<leader>cn", false},
		},
	},

	{
		"MagicDuck/grug-far.nvim",
		keys = { { "<leader>sr", false } }
	},

}

if IsPluginUsed("copilot-cmp") then
	table.insert(spec,
		{
			"nvim-cmp",
			opts = function(_, opts)
				for _, v in pairs(opts.sources) do
					if v.name == "copilot" then
						v.priority = 1
						break
					end
				end

				local cmp = require("cmp")
				opts.sorting = {
					priority_weight = 1,
					comparators = {
						-- Below is the default comparitor list and order for nvim-cmp
						cmp.config.compare.offset,
						-- cmp.config.compare.scopes, --this is commented in nvim-cmp too
						cmp.config.compare.exact,
						require("copilot_cmp.comparators").prioritize,
						cmp.config.compare.score,
						cmp.config.compare.recently_used,
						cmp.config.compare.locality,
						cmp.config.compare.kind,
						cmp.config.compare.sort_text,
						cmp.config.compare.length,
						cmp.config.compare.order,
					},
				}
			end,
		})
end

return spec
