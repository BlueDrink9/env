-- call SourcePluginFile("nvim-lspconfig.lua")
-- call SourcePluginFile('nvim-cmp.lua')

local idemaps = vim.g.IDE_mappings

local lazyvim_extras = {
	{ import = "lazyvim.plugins.extras.coding.yanky"},
	{ import = "lazyvim.plugins.extras.coding.luasnip"},
	{ import = "lazyvim.plugins.extras.dap.core"},
	{ import = "lazyvim.plugins.extras.dap.nlua"},

	{ import = "lazyvim.plugins.extras.coding.copilot"},
	{ import = "lazyvim.plugins.extras.coding.copilot-chat" },
	-- { import = "lazyvim.plugins.extras.coding.codeium" },

	-- { import = "lazyvim.plugins.extras.lang.python", cond = IsPluginUsed("LazyVim") },
	{ import = "lazyvim.plugins.extras.lang.rust"},
	{ import = "lazyvim.plugins.extras.lang.yaml"},
	{ import = "lazyvim.plugins.extras.lang.json"},

	{ import = "lazyvim.plugins.extras.test.core"},

	{ import = "lazyvim.plugins.extras.formatting.prettier"},
	{ import = "lazyvim.plugins.extras.coding.neogen" },
	{ import = "lazyvim.plugins.extras.util.project"},

	-- { import = "lazyvim.plugins.extras.lsp.none-ls", cond = IsPluginUsed("LazyVim") },

}
for k, _ in pairs(lazyvim_extras) do
	lazyvim_extras[k].enabled=IsPluginUsed("LazyVim")
end

return {
	lazyvim_extras,

	{ import = "plugins.cmp" },
	{ import = "plugins.lspconfig" },
	{ import = "plugins.nvim-dap" },
	{ import = "plugins.telescope" },
	
	{ import = "plugins.repl"},

	-- Dependency for a lot of plugins
	{ "nvim-lua/plenary.nvim", lazy = true },

	-- For installing LSPs (and other packages)
	{
		"williamboman/mason.nvim",
		cmd = "Mason",

		opts = function(_, opts)
			local to_install = {
				"pyright",
				"debugpy",
				"ruff",
				"ruff-lsp",
				"stylua",
				"lua-language-server",
				"luacheck",
				"shfmt",
				"shellcheck",
				"prettier",
				"black",
				"pylint",
				"jq",
				"jq-lsp",
				"markdownlint",
				"sql-formatter",
				"sqlfluff",
				"sqlfmt",
				"sqls",
				"textlint",
				-- "codelldb",
				-- "rust-analyser",
			}

			-- Alternative to ensure_installed, that only runs when I ask it to.
			-- Gives convenience of set list of tools, but without the risk of errors on systems that fail to install defaults.
			vim.api.nvim_create_user_command(
				"MasonInstallMine",
				-- Best practice is to check/refresh the package registry before installing
				function()
					require("mason-registry").refresh(
						function () vim.cmd.MasonInstall(to_install) end
					)
				end,
			{})

			override_opts = {
				ensure_installed=false,
				ui = {
					icons = {
						package_installed = "✓",
						package_pending = "➜",
						package_uninstalled = "✗",
					},
				},
			}
			vim.tbl_deep_extend("keep", override_opts, opts)
		end,

		dependencies = {
			"https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim"
		},
	},

	{ "nvchad/nvim-colorizer.lua", config = true, event = "VeryLazy" },
	-- {'nvim-notify', opt={stages="static"}},

	-- {[} ---------- Visual ----------
	{
		"folke/noice.nvim",
		enabled = false,
		opts = {
			cmdline = {
				-- view = "cmdline",  -- or "cmdline_popup" for fancy.
				-- Sent to nui.nvim's input popup
				input = {
					-- input:map("n", "<esc>", input.input_props.on_close, { noremap = true })
				},
			},
			popupmenu = {
				enabled = true,
			},
			presets = {
				command_palette = true,
				long_message_to_split = true,
			},
			messages = {
				view_search = false,
			},
			lsp = {
				progress = {
					enabled = false,
				},
			},
		},
	},
	-- override my normal mapping to ctrl-f, since noice can handle it like normal
	-- vim.api.nvim_create_autocmd(
	-- "VimEnter",
	--  {
	--   group = "myIDE",
	--   buffer = bufnr,
	--   callback = function()
	--      vim.api.nvim_set_keymap('c', "kv", "<esc>", { noremap=true, silent=true })
	--      vim.api.nvim_set_keymap('c', "vk", "<esc>", { noremap=true, silent=true })
	--   end,
	--  })

	{
		"https://github.com/lukas-reineke/indent-blankline.nvim.git",
		main = "ibl",
		opts = {
			indent = {
				char = "┆",
				tab_char = "┆",
				-- char_blankline = "",
				highlight = {
					"IndentBlanklineIndent1",
					"IndentBlanklineIndent2",
					"IndentBlanklineIndent3",
					"IndentBlanklineIndent4",
					"IndentBlanklineIndent5",
				},
			},
			-- May be a touch slow
			scope = {
				show_start = true,
				show_exact_scope = true,
				-- show_end = false,
			},
		},
		config = function(_, opts)
			opts.scope.enabled = IsPluginUsed("nvim-treesitter")

			local hooks = require("ibl.hooks")
			hooks.register(hooks.type.WHITESPACE, hooks.builtin.hide_first_space_indent_level)
			hooks.register(hooks.type.WHITESPACE, hooks.builtin.hide_first_tab_indent_level)

			local indentcolours = {}
			if vim.opt.termguicolors then
				-- Auto-gen some greys
				for i = 100, 201, (200 - 100) / 5 do
					table.insert(indentcolours, string.format("#%02x%02x%02x", i, i, i))
				end
			else
				indentcolours = {
					"Red",
					"Yellow",
					"Green",
					"Blue",
					"Purple",
				}
			end

			hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
				for i, colour in pairs(indentcolours) do
					vim.api.nvim_set_hl(0, "IndentBlanklineIndent" .. i, { fg = colour, nocombine = true })
				end
			end)

			require("ibl").setup(opts)
		end,
	},

	{
		"folke/which-key.nvim",
		opts = {
			-- triggers_blacklist = {
			-- 	i = { "k", "v", "(", "{", "[" },
			-- },
			-- Don't notify when issues/conflicts detected with mappings
			-- (can still check with checkhealth, but lots are intentional)
			notify = false,
		},
		event = "VeryLazy",
	},

	{ "sudormrfbin/cheatsheet.nvim", cmd = "Cheatsheet", keys = { "<leader><s-/>", "<cmd>Cheatsheet<cr>" } },

	{
		"https://github.com/winston0410/range-highlight.nvim",
		dependencies = { "winston0410/cmd-parser.nvim" },
		event = "CmdlineEnter",
		config = true,
	},

	-- {'mfussenegger/nvim-lint', config = function()
	--   vim.api.nvim_create_autocmd({ "BufWritePost" }, {
	--     callback = function()
	--       require("lint").try_lint()
	--     end,
	--     group='myIDE',
	--   })
	-- end
	-- },

	-- {]} ---------- Visual ----------

	-- -- Haven't configured yet. Add the none-ls extra
	-- {
	-- 	"nvimtools/none-ls.nvim",
	-- 	-- will setup any installed and configured sources for mason
	-- 	opts = {
	-- 		sources = {
	-- 			-- null-ls.builtins.code_actions.refactoring,
	-- 			-- require("null-ls").builtins.completion.spell,
	-- 			-- require'null-ls'.builtins.hover.printenv,
	-- 		},
	-- 	},
	-- },

	-- {
	-- 	"jay-babu/mason-null-ls.nvim",
	-- 	event = { "BufReadPre", "BufNewFile" },
	-- 	dependencies = {
	-- 		"williamboman/mason.nvim",
	-- 		"nvimtools/none-ls.nvim",
	-- 	},
	-- 	opts = {
	-- 		-- ensure_installed = {
	-- 		--   'vint',
	-- 		--   'selene',
	-- 		--   'stylua',
	-- 		--   'pylint',
	-- 		--   'shellcheck',
	-- 		--   'jq',
	-- 		--   'proselint',
	-- 		-- },
	-- 		automatic_setup = true,
	-- 	},
	-- 	-- config = function(_, opts)
	-- 	--     require("mason-null-ls").setup(opts)
	-- 	-- end,
	-- },

	-- {'simrat39/symbols-outline.nvim', config=true},

	{
		"NeogitOrg/neogit",
		opts = {
			disable_commit_confirmation = true,
			disable_insert_on_commit = "auto",
			disable_builtin_notifications = vim.fn.has("win32") == 1,
			disable_line_numbers = false,
			mappings = {
				finder = {
					["<BS>"] = "MultiselectToggleNext",
				},
				-- Setting any of these to `false` will disable the mapping.
				status = {
					["<BS>"] = "Toggle",
				},
			},
		},
		config = function(_, opts)
			vim.api.nvim_create_user_command("Magit", "Neogit", {})
			-- For some reason lazy isn't mapping this right.
			vim.keymap.set("n", "<leader>gg", "<cmd>Neogit<CR>")
			require("neogit").setup(opts)
		end,
		cmd = { "Neogit", "Magit" },
		keys = { "<leader>gg", "<cmd>Neogit<CR>", desc = "NeoGit" },
		dependencies = {
			"nvim-lua/plenary.nvim", -- required
			"nvim-telescope/telescope.nvim", -- optional
			"sindrets/diffview.nvim", -- optional
			-- "ibhagwan/fzf-lua",              -- optional
		},
		tag=vim.fn.has("nvim-0.10") == 0 and "v0.0.1" or nil,
	},

		-- dependencies = {
		-- 	{ "jalvesaq/cmp-nvim-r", enabled=IsPluginUsed("cmp.nvim") },
		-- 	-- https://github.com/jalvesaq/cmp-zotcite
		-- 	-- https://github.com/jalvesaq/zotcite
		-- },

	-- Switching to luasnip via lazyvim
	-- {
	--  "https://github.com/hrsh7th/vim-vsnip",
	--  keys = {
	--      { idemaps.snippetExpand, "<Plug>(vsnip-expand-or-jump)", mode = { "i", "v" } },
	--  },
	--  config = function()
	--      vim.g.vsnip_snippet_dir = vim.fn.PathExpand(vim.g.plugin_config_dir .. "/../runtimepath/snippets")
	--  end,
	--  event = "InsertEnter",
	-- },
	-- { "hrsh7th/vim-vsnip-integ", event = "InsertEnter" },

	{
		"L3MON4D3/LuaSnip",
		init = function()
			vim.g.snippet_dir = vim.fn.PathExpand(vim.g.configDir  .. "/runtimepath/snippets")
			vim.api.nvim_create_user_command("SnipEdit", function(opts)
					args = opts.args or nil
					local snipPath = vim.fn.PathExpand(vim.g.snippet_dir .. "/" .. vim.bo.filetype .. ".json")
					vim.cmd(":edit " .. snipPath)
				end,
				{}
			)
		end,
		keys = {
			{ "<C-e>", function()
				if require("luasnip").expand_or_locally_jumpable() then
					require("luasnip").expand_or_jump()
				else
					return "<C-e>"
				end
				end,
				expr = true, silent = true, mode = "i", }
		},
		opts = function(_, opts)
			-- load snippets from path/of/your/nvim/config/my-cool-snippets
			require("luasnip.loaders.from_snipmate").lazy_load({
				paths = { vim.g.snippet_dir },
			})
			require("luasnip.loaders.from_vscode").lazy_load({
				paths = { vim.g.snippet_dir },
			})
		end,
	},

	-- build123d snippets, vscode formatted
	{
		url="https://gist.github.com/6233ac42632d9098c99da029fe0615c2.git",
		name="build123d-OCP.code-snippets",
		ft="python",
		build=function(_)
			local package_json = vim.json.encode({
				name= "build123d-OCP.code-snippets",
				engines= {
					vscode= "^1.11.0"
				},
				contributes= {
					snippets= {
						{
							language= {
								"python",
							},
							path= "./build123d-OCP.code-snippets/snippets"
						}
					}
				}
			})
			local file = io.open(vim.g.pluginInstallPath ..
				"/build123d-OCP.code-snippets/package.json", "w")
			 if file then
				 file:write(package_json)
				 file:close()
			end
		end
	},

	{ "polypus74/trusty_rusty_snippets", ft="rust"},

	{ "octaltree/virtualsnip", build = "make", event = "InsertEnter" },

	{
		"Zeioth/compiler.nvim",
		cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
		dependencies = { "stevearc/overseer.nvim" },
		opts = {},
	},

	{ -- The task runner for compiler
		"stevearc/overseer.nvim",
		commit = "19aac0426710c8fc0510e54b7a6466a03a1a7377",
		cmd = { "CompilerOpen", "CompilerToggleResults", "CompilerRedo" },
		opts = {
			task_list = {
				direction = "bottom",
				min_height = 25,
				max_height = 25,
				default_detail = 1,
				bindings = {
					["q"] = function()
						vim.cmd("OverseerClose")
					end,
				},
			},
		},
	},

	{ -- docs generator
		"kkoomen/vim-doge",
		build = ":call doge#install()",
		cmd = "DogeGenerate",
	},

	{
		"bennypowers/nvim-regexplainer",
		opts = {
			mappings = {
				toggle = "<leader>hR",
			},
		},
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"MunifTanjim/nui.nvim",
		},
		keys = { "<leader>hR" },
		cmd = {
			"RegexplainerToggle",
			"RegexplainerShowSplit",
			"RegexplainerShowPopup",
		},
	},

	{
		"stevearc/conform.nvim",
		opts = {
			formatters_by_ft = {
				python = { "isort", "black" },
			},
		},
	},

	{
		"zbirenbaum/copilot.lua",
		cmd = "Copilot",
		event = "InsertEnter",
		opts = {
			suggestion = {
				keymap = {
					accept = "<C-j>",
					next = "<M-j>",
					previous = "<M-k>",
				}
			},
			panel = {
				keymap = {
					open = "<M-e>",
				}
			}
		},
	},

	{
		'linux-cultist/venv-selector.nvim',
		dependencies = { 'neovim/nvim-lspconfig', 'nvim-telescope/telescope.nvim' },
		opts = {
			name = {"venv", ".venv"},
			auto_refresh = true,
			dap_enabled = true,
		},
		cmd = { "VenvSelect", "VenvSelectCached" },
	},

	{ "quarto-dev/quarto-nvim",
		ft = {"qmd", "quarto", "ipynb"},

		dependencies = {
			"jmbuhr/otter.nvim",
			"nvim-treesitter/nvim-treesitter",
			{ 'GCBallesteros/jupytext.nvim',
				-- directly open ipynb files as quarto docuements
				-- and convert back behind the scenes
				opts = {
					custom_language_formatting = {
						python = {
							extension = 'qmd',
							style = 'quarto',
							force_ft = 'quarto',
						},
						r = {
							extension = 'qmd',
							style = 'quarto',
							force_ft = 'quarto',
						},
					},
				},
			},

		},

	},



}
