-- skip loading lazyvim options
package.loaded["lazyvim.config.options"] = true

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
		missing = false, -- install missing plugins on startup.

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
				"tohtml",
				"tutor",
				"zipPlugin",
			},
		},
	},
	change_detection = {
		notify = false,
	},
	spec = {

		-- add LazyVim and import its plugins
		{
			"LazyVim/LazyVim",
			import = "lazyvim.plugins",
			version = "^6.2.0",
			opts = {
				colorscheme = "ayu",
				defaults = {
					autocmds = false,
					keymaps = false,
				},
			},
		},

		{ import = "lazyvim.plugins.extras.editor.leap" },

		{ import = "lazyvim.plugins.extras.lsp.none-ls" },

		{ import = "plugins" },

		LazyPlugSpecs,

		{ "https://github.com/Shatur/neovim-ayu", lazy = true },
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
