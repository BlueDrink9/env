local lazypath = vim.g.pluginInstallPath .. "/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  -- bootstrap lazy.nvim
  -- stylua: ignore
  vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath })
end
vim.opt.rtp:prepend(vim.env.LAZY or lazypath)

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
    missing = true, -- install missing plugins on startup.
    colorscheme = { "ayu", }, -- "tokyonight", "habamax" }
  },
  checker = { enabled = false }, -- don't automatically check for plugin updates
  performance = {
    reset_packpath = true, -- reset the package path to improve startup time
    rtp = {
      reset = false,  -- reset the runtime path to $VIMRUNTIME and your config directory
      -- disable some rtp plugins
      disabled_plugins = {
        "gzip",
        -- "matchit",
        -- "matchparen",
        -- "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
  spec = {
    -- add LazyVim and import its plugins
    { "LazyVim/LazyVim",
      import = "lazyvim.plugins",
      version = '^6.2.0',
      opts = {
        colorscheme = "ayu",
        defaults = {
          autocmds = false,
          keymaps = false,
        },
      }
    },

    -- { import = "lazyvim.plugins.extras.coding.yanky" },
    -- { import = "lazyvim.plugins.extras.dap.core" },
    -- { import = "lazyvim.plugins.extras.dap.nlua" },

    -- { import = "lazyvim.plugins.extras.lang.python" },
    -- { import = "lazyvim.plugins.extras.lang.rust" },
    -- { import = "lazyvim.plugins.extras.lang.yaml" },
    -- { import = "lazyvim.plugins.extras.lang.json" },

    -- { import = "lazyvim.plugins.extras.test.core" },

    -- { import = "lazyvim.plugins.extras.formatting.prettier" },
    -- { import = "lazyvim.plugins.extras.util.project" },

    { import = "lazyvim.plugins.extras.editor.leap" },
    -- import any extras modules here
    -- { import = "lazyvim.plugins.extras.lang.typescript" },
    -- { import = "lazyvim.plugins.extras.lang.json" },
    -- { import = "lazyvim.plugins.extras.ui.mini-animate" },

    { import = "plugins" },

    MyLazySpecs,

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
vim.cmd[[abbrev packi Lazy install]]
vim.cmd[[abbrev packu Lazy update]]
