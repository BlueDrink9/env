require('refactoring').setup({})
local maps = vim.api.nvim_get_var('IDE_mappings')
-- prompt for a refactor to apply when the remap is triggered
-- if using telescope, we replace it with its mapping
if vim.fn.IsPluginUsed("telescope.nvim") == 0 then
   for _, mode in pairs({'v', 'n'}) do
      vim.api.nvim_set_keymap(
         mode,
         maps.refactor,
         ":lua require('refactoring').select_refactor()<CR>",
         { noremap = true, silent = true, expr = false }
         )
   end
end

-- generate annotations (eg docstrings)
require('neogen').setup {}
vim.cmd("command! Annotate lua require('neogen').generate()")

require('nvim-treesitter.configs').setup {
   auto_install = false,
   ensure_installed = {
       "vim",
       "lua",
       "bash",
       -- "powershell", -- not available yet
       "markdown",
       "make",
       "json",
       "yaml",
       "toml",
       "python",
       "r",
       "sql",
   },
   endwise = { enable = true, },
   indent = {
      enable = true,
      disable = {"python"},
   },
   highlight = {
      enable = true,
      -- additional_vim_regex_highlighting = {"python"}
   },
}
vim.opt.foldexpr="nvim_treesitter#foldexpr()"

require'nvim-treesitter.configs'.setup {
   context_commentstring = {
      enable = true
   }
}
require'treesitter-context'.setup{}

require'nvim-treesitter.configs'.setup {
   textobjects = {
      select = {
         enable = true,
         -- Automatically jump forward to textobj, similar to targets.vim
         lookahead = true,
         keymaps = {
           ["a,"] = "@parameter.outer",
           ["i,"] = "@parameter.inner",
           ["af"] = "@function.outer",
           ["if"] = "@function.inner",
           ["ac"] = "@class.outer",
           ["ic"] = "@class.inner",
           -- { query = "@class.inner", desc = "Select inner part of a class region" }
         },
         selection_modes = {
            ['@parameter.outer'] = 'v', -- charwise
            ['@function.outer'] = 'V', -- linewise
            ['@class.outer'] = 'V',
         },
         -- Can also be a function which gets passed a table with the keys
         -- * query_string: eg '@function.inner'
         -- * selection_mode: eg 'v'
         -- and should return true of false
         include_surrounding_whitespace = true,
      },
      swap = {
         enable = true,
         swap_next = {
            [">,"] = "@parameter.inner",
         },
         swap_previous = {
            ["<,"] = "@parameter.inner",
         },
      },
   },
   move = {
      enable = true,
      set_jumps = true,
      goto_next_start = {
         ["]m"] = "@function.outer",
         ["]]"] = { query = "@class.outer", desc = "Go to next class start" },
         ["],"] = { query = "@parameter.inner", desc = "Go to next argument" },
      },
      goto_next_end = {
         ["]M"] = "@function.outer",
         ["]["] = "@class.outer",
      },
      goto_previous_start = {
         ["[m"] = "@function.outer",
         ["[["] = "@class.outer",
         ["[,"] = { query = "@parameter.inner", desc = "Go to previous argument" },
      },
      goto_previous_end = {
         ["[M"] = "@function.outer",
         ["[]"] = "@class.outer",
      },
   },
}
-- require('treesj').setup({
--    use_default_keymaps = false,
-- })

if vim.fn.IsPluginUsed('nvim-ufo') == 1 then
    -- vim.o.foldcolumn = '1' -- '0' is not bad
    -- Have to override foldlevelstart, otherwise constantly closes folds on save.
    vim.o.foldlevelstart = 99
    vim.o.foldenable = true

    -- Using ufo provider need remap `zR` and `zM`. If Neovim is 0.6.1, remap yourself
    vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
    vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)

   require('ufo').setup({
       provider_selector = function(bufnr, filetype, buftype)
           return {'treesitter', 'indent'}
       end
   })
end
