if not IsPluginUsed("LazyVim") then
	return {}
end

local spec = {
	{
		"LazyVim/LazyVim",
		opts = {
			defaults = {
				keymaps = false,
			},
		},
		init = function()
			vim.g.autoformat = false
			vim.g.deprecation_warnings = true
			pcall(vim.keymap.del, "n", "<leader>c")
			pcall(vim.keymap.del, "n", "<leader>s")
		end,
	},

	{
		"neovim/nvim-lspconfig",
		cond = vim.g.ideMode == 1,
		opts = function(_, opts)
			-- Ensure we use my default config rather than lazyvim's.
			opts.diagnostics = vim.diagnostic.config()
			-- Prefix with icon, depending on severity
			if vim.g.useNerdFont == 1 and vim.fn.has("nvim-0.10") == 1 then
				-- opts.diagnostics.virtual_text.prefix = "icons"
				DiagnosticVirtualTextPrefix = function(diagnostic)
					local symbols = {
						require("lazyvim.util").config.icons.diagnostics.Error,
						require("lazyvim.util").config.icons.diagnostics.Warn,
						require("lazyvim.util").config.icons.diagnostics.Hint,
						require("lazyvim.util").config.icons.diagnostics.Info,
					}
					return symbols[diagnostic.severity]
				end
			end
			local keys = require("lazyvim.plugins.lsp.keymaps").get()
			-- Clear default lsp keymaps
			-- for i=0, #keys do keys[i]=nil end
			keys[#keys + 1] = { "<leader>ca", false, mode = { "v", "n" } }
			keys[#keys + 1] = { "<leader>cc", false, mode = { "v", "n" } }
			keys[#keys + 1] = { "<leader>cs", false, mode = { "v", "n" } }
			keys[#keys + 1] = { "<c-k>", false, mode = { "i" } }
		end,
	},

	{
		"conform.nvim",
		cond = vim.g.ideMode == 1,
		keys = { { "<leader>cF", false, mode = { "v", "n" } } },
	},

	{
		"nvim-treesitter/nvim-treesitter",
		cond = vim.g.liteMode == 0,
		opts = {
			ensure_installed = false,
			auto_install = false,
		},
	},

	{
		"mason-org/mason.nvim",
		opts_extend = {},
		opts = {
			ensure_installed = {},
		},
	},

	{
		"nvim-mini/mini.surround",
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
		"nvim-mini/mini.comment",
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
		"nvim-mini/mini.ai",
		opts = function(_, opts)
			opts.mappings = vim.tbl_deep_extend("force", opts.mappings or {}, {
				around = "a",
				inside = "i",
				around_next = "",
				inside_next = "",
				around_last = "",
				inside_last = "",
			})
			opts.custom_textobjects = opts.custom_textobjects or {}
			opts.custom_textobjects.C = function()
				return require("mini.ai").config.custom_textobjects.c
			end
			return opts
		end,
		keys = {
			{ "a", mode = "o" },
			{ "i", mode = "o" },
		},
	},

	{
		"folke/which-key.nvim",
		cond = vim.g.ideMode == 1,
		opts = {
			plugins = { spelling = true },
			spec = {
				{
					mode = { "x", "v" },
					-- Remove these because they begin operator funcs, which get overritten
					-- by which-key here if lazy-loaded. (E.g. comment)
					{ "<leader>c", nil },
					{ "<leader>s", nil },
					-- {"<leader>u",  nil},
				},
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
		"nvim-lualine/lualine.nvim",
		enabled = vim.g.liteMode == 0,
	},

	{
		"folke/todo-comments.nvim",
		enabled = vim.g.ideMode == 1,
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
			{ "<leader>cn", false },
		},
	},

	{
		"MagicDuck/grug-far.nvim",
		keys = { { "<leader>sr", false, mode = { "n", "v" } } },
	},

	{
		"akinsho/bufferline.nvim",
		keys = {
			{ "<S-h>", false },
			{ "<S-l>", false },
			{ "<left>", "<cmd>BufferLineCyclePrev<cr>", desc = "Prev Buffer" },
			{ "<right>", "<cmd>BufferLineCycleNext<cr>", desc = "Next Buffer" },
		},
	},

	-- {
	--     "folke/noice.nvim",
	--     opts = {
	--       -- Keeps the command line at the very bottom where it belongs
	--       cmdline = { view = "cmdline" },
	--       -- Lets Noice catch the "Press Enter" warnings and route them to Snacks
	--       messages = { enabled = true },
	--       -- Disables the annoying popup when you search with '/'
	--       search = { view = "cmdline" },
	--     },
	--   },
}

for _, s in ipairs(spec) do
	s.optional = true
end

return spec
