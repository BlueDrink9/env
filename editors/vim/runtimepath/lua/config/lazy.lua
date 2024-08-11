-- skip loading lazyvim options
package.loaded["lazyvim.config.options"] = true

-- Blank strings are ignored so this is a good way to conditionally disable
-- default plugins.
local extra_disabled_builtin_plugin_1 = vim.g.liteMode == 1 and "rplugin" or ""
local extra_disabled_builtin_plugin_2 = vim.g.liteMode == 1 and "editorconfig" or ""
local extra_disabled_builtin_plugin_3 = vim.g.liteMode == 1 and "health" or ""

-- LazyVim disable autoformat
vim.g.autoformat = false

require("lazy").setup({
	root = vim.g.pluginInstallPath, -- directory where plugins will be installed
	defaults = {
		-- By default, only LazyVim plugins will be lazy-loaded. Your custom plugins will load during startup.
		-- If you know what you're doing, you can set this to `true` to have all your custom plugins lazy-loaded by default.
		lazy = false,
		version = false, -- always use the latest git commit
		-- version = "*", -- try installing the latest stable version for plugins that support semver
	},
	install = {
		missing = vim.g.plugins_first_install and vim.g.plugins_first_install == 1 or false, -- install missing plugins on startup.

		-- Colourschemes for installing missing on startup
		colorscheme = { vim.g.colorSch, "ayu", vim.g.defaultColorSch, "default" },
	},
	checker = { enabled = false }, -- don't automatically check for plugin updates
	performance = {
		reset_packpath = true, -- reset the package path to improve startup time
		rtp = {
			reset = true, -- reset the runtime path to $VIMRUNTIME and your config directory
			paths = { vim.g.configDir .. vim.fn.expand("/runtimepath") },
			-- disable some rtp plugins
			disabled_plugins = {
				"gzip",
				"matchit",
				"matchparen",
				"netrwPlugin",
				"tarPlugin",
				-- "tohtml",
				"tutor",
				"zipPlugin",
				extra_disabled_builtin_plugin_1,
				extra_disabled_builtin_plugin_2,
				extra_disabled_builtin_plugin_3,
			},
		},
	},
	change_detection = {
		notify = false,
	},
	-- Enable profiling of lazy.nvim. This will add some overhead,
	-- so only enable this when you are debugging lazy.nvim
	profiling = {
		-- Enables extra stats on the debug tab related to the loader cache.
		-- Additionally gathers stats about all package.loaders
		loader = false,
		-- Track each new require in the Lazy profiling tab
		require = false,
	},
	spec = {
		{ "dstein64/vim-startuptime", cond = vim.g.liteMode == 0 },

		-- add LazyVim and import its plugins
		{
			"LazyVim/LazyVim",
			import = "lazyvim.plugins",
			version = "12.*",
			opts = {
				colorscheme = "ayu",
				defaults = {
					autocmds = false,
					keymaps = false,
				},
			},
			cond = vim.g.liteMode == 0,
			init = function()
				vim.g.autoformat = false
			end,
		},


		{ import = "plugins.light" },
		{ import = "plugins.main", cond = vim.g.liteMode == 0 },
		{ import = "plugins.ide", cond = vim.g.ideMode == 1 },

		LazyPlugSpecs,

		{ "https://github.com/Shatur/neovim-ayu", lazy = true },

		{ import = "plugins.lazyvim_disabled", cond = vim.g.liteMode == 0  },
		{ import = "plugins.lazyvim_override", cond = vim.g.liteMode == 0  },
	},
	-- Unicode alternatives
	ui = {
		icons = {
			cmd = "⌘",
			config = "🛠",
			event = "📅",
			ft = "📂",
			init = "⚙",
			keys = "🗝",
			plugin = "🔌",
			runtime = "💻",
			source = "📄",
			start = "🚀",
			task = "📌",
			lazy = "💤 ",
		},
	},
})
vim.cmd([[cabbrev packi Lazy install]])
vim.cmd([[cabbrev packu Lazy update]])
