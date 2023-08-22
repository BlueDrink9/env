local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
local do_install = false
if not vim.loop.fs_stat(lazypath) then
  -- bootstrap lazy.nvim
  -- stylua: ignore
  vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath })
  do_install = true
end
vim.opt.rtp:prepend(vim.env.LAZY or lazypath)

require("lazy").setup({
  spec = {
    -- add LazyVim and import its plugins
    { "LazyVim/LazyVim", import = "lazyvim.plugins" },

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
  defaults = {
    -- By default, only LazyVim plugins will be lazy-loaded. Your custom plugins will load during startup.
    -- If you know what you're doing, you can set this to `true` to have all your custom plugins lazy-loaded by default.
    lazy = false,
    version = false, -- always use the latest git commit
    -- version = "*", -- try installing the latest stable version for plugins that support semver
  },
  install = {
    missing = true, -- don't install missing plugins on startup.
    colorscheme = { "ayu", }, -- "tokyonight", "habamax" }
  },
  checker = { enabled = false }, -- don't automatically check for plugin updates
  performance = {
    rtp = {
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
  -- Unicode alternatives
  -- ui = {
  --   icons = {
  --     cmd = "⌘",
  --     config = "🛠",
  --     event = "📅",
  --     ft = "📂",
  --     init = "⚙",
  --     keys = "🗝",
  --     plugin = "🔌",
  --     runtime = "💻",
  --     source = "📄",
  --     start = "🚀",
  --     task = "📌",
  --     lazy = "💤 ",
  --   },
  -- },
})
vim.cmd[[abbrev packi Lazy install]]
vim.cmd[[abbrev packu Lazy update]]
