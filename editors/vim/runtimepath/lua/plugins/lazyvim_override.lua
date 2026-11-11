if not IsPluginUsed("LazyVim") then
  return {}
end

local maps = vim.g.IDE_mappings or {}

local spec = {
    {
        "LazyVim/LazyVim",
        opts = {
            defaults = {
                keymaps = false,
            },
        },
        keys = {
            { maps.FuzzyOldFiles, function() LazyVim.pick("oldfiles")() end, desc = "Recent Files (MRU)" },
            { maps.FuzzySearchFiles, function() LazyVim.pick("grep")() end, desc = "Grep Workspace (Root Dir)" },
        },
        init = function()
            vim.g.autoformat = false
            vim.g.deprecation_warnings = true
        end,
    },

    {
        "folke/which-key.nvim",
        cond = vim.g.ideMode == 1,
        -- Declaratively disable LazyVim's default buffer keymap takeover
        keys = {
            { maps.FuzzySearchBuffers, false },
            { maps.FuzzySearchBuffer, false },
        },
        opts = {
            -- Don't modify timeout during runtime and this should stay fine. Use OptionSet autocmd if that needs to change.
            delay = vim.o.timeoutlen + 50,
            plugins = { spelling = true },
            spec = {
                {
                    mode = { "n", "x", "v" },
                    -- Prevent Which-Key from treating these as folder menus
                    { maps.FuzzySearchBuffer, group = false },
                    { maps.FuzzySearchBuffers, group = false },
                    { "<leader>c", group = false },
                    { "<leader>s", group = false },
                    { "<leader>?", group = false },
                },
            },
        },
    },

    {
        "folke/snacks.nvim",
        keys = {
            -- Disarm LazyVim's Grep (Root Dir) and bind to your buffer lines
            { maps.FuzzySearchBuffer, function() Snacks.picker.lines() end, desc = "Search Current Buffer (BLines)" },
        },
    },

    {
        "neovim/nvim-lspconfig",
        cond = vim.g.ideMode == 1,
        opts = function(_, opts)
            opts.diagnostics = vim.diagnostic.config()
            if vim.g.useNerdFont == 1 and vim.fn.has("nvim-0.10") == 1 then
                DiagnosticVirtualTextPrefix = function(diagnostic)
                    local icons = LazyVim.config.icons.diagnostics
                    local symbols = {
                        icons.Error,
                        icons.Warn,
                        icons.Hint,
                        icons.Info,
                    }
                    return symbols[diagnostic.severity]
                end
            end
            opts.servers = opts.servers or {}
            opts.servers["*"] = opts.servers["*"] or {}
            opts.servers["*"].keys = {
                { "<leader>ca", false, mode = { "n", "v" } },
                { "<leader>cc", false, mode = { "n", "v" } },
                { "<leader>cC", false, mode = { "n", "v" } },
                { "<leader>cd", false, mode = { "n", "v" } },
                { "<leader>cf", false, mode = { "n", "v" } },
                { "<leader>cr", false, mode = { "n", "v" } },
                { "<leader>cl", false, mode = { "n", "v" } },
                { "<leader>cs", false, mode = { "n", "v" } },
                { "<c-k>", false, mode = { "i" } },
            }
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
                -- Move 'last' from al/il to aL/iL so Neovim 0.13 native al/il works
                around_last = "aL",
                inside_last = "iL",
                -- Move 'next' from an/in to aN/iN so Neovim 0.12+ incremental select works
                around_next = "aN",
                inside_next = "iN",
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
        "nvim-ts-autotag",
        ft = {
            "astro", "glimmer", "handlebars", "html", "javascript",
            "jsx", "markdown", "php", "rescript", "svelte", "tsx",
            "typescript", "vue", "xml",
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

    {
        "folke/noice.nvim",
        opts = {
            views = {
                cmdline_popup = { win_options = { winblend = 0 } },
                popup         = { win_options = { winblend = 0 } },
                mini          = { win_options = { winblend = 0 } },
                split         = { win_options = { winblend = 0 } },
                hover         = { win_options = { winblend = 0 } },
            },
        },
        config = function(_, opts)
            -- Keeps the command line at the very bottom where it belongs
            -- cmdline = { view = "cmdline" },
            -- Lets Noice catch the "Press Enter" warnings and route them to Snacks
            -- messages = { enabled = true },
            -- Disables the annoying popup when you search with '/'
            -- search = { view = "cmdline" },

            require("noice").setup(opts)

            local function link_noice_highlights()
                local links = {
                    NoiceCmdlinePopup       = "NormalFloat",
                    NoiceCmdlinePopupBorder = "FloatBorder",
                    NoiceCmdlinePrompt      = "Title",
                    NoiceMini               = "NormalFloat",
                    NoiceConfirm            = "NormalFloat",
                    NoiceConfirmBorder      = "FloatBorder",
                    NotifyBackground        = "NormalFloat",
                    NotifyERRORBorder       = "DiagnosticError",
                    NotifyWARNBorder        = "DiagnosticWarn",
                    NotifyINFOBorder        = "DiagnosticInfo",
                    NotifyDEBUGBorder       = "Debug",
                    NotifyTRACEBorder       = "Comment",
                    NotifyERRORIcon         = "DiagnosticError",
                    NotifyWARNIcon          = "DiagnosticWarn",
                    NotifyINFOIcon          = "DiagnosticInfo",
                }

                for group, target in pairs(links) do
                    vim.api.nvim_set_hl(0, group, { link = target, default = true })
                end
            end

            link_noice_highlights()
            vim.api.nvim_create_autocmd("ColorScheme", {
                group = vim.api.nvim_create_augroup("NoiceThemeLinks", { clear = true }),
                callback = link_noice_highlights,
            })
        end,
    },

    {
        "rcarriga/nvim-notify",
        opts = {
            stages = "static", -- Prevents the alpha calculation that washes out borders
            timeout = 3000,
        },
    }
}

for _, s in ipairs(spec) do
    s.optional = true
end

return spec
