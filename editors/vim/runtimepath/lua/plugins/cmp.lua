return {
  -- first: disable LuaSnip bindings that conflict
  { "L3MON4D3/LuaSnip", keys = function() return {} end, },
  
  {
    'hrsh7th/nvim-cmp',
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.preselect = cmp.PreselectMode.None;
      vim.o.completeopt = "menu,menuone,noselect,noinsert"

      vim.g.cmptoggle = true
      opts.enabled = function()
        local buftype = vim.api.nvim_get_option_value("buftype", {buf=0})
        if buftype == "prompt" then return false end
        return vim.g.cmptoggle
      end
      -- Create a quick way to toggle cmp
      local toggle_cmp = function() vim.g.cmptoggle = not vim.g.cmptoggle end
      vim.keymap.set("n", "<leader>tC", toggle_cmp, { desc = "toggle nvim-cmp" })
      vim.api.nvim_create_user_command('CmpToggle', toggle_cmp, {})


      opts.completion = {
        completeopt = "noselect",
      }

      local get_bufnrs_to_complete_from = function()
        -- Include all visible buffers (not just default of current one) below a certain size.
        local bufs = {}
        for _, win in ipairs(vim.api.nvim_list_wins()) do
          local buf = vim.api.nvim_win_get_buf(win)
          local byte_size = vim.api.nvim_buf_get_offset(
            buf, vim.api.nvim_buf_line_count(buf))
          if byte_size < 1024 * 1024 then -- 1 Megabyte max
            bufs[buf] = true
          end
        end
        return vim.tbl_keys(bufs)
      end

      local sources = {
        -- { name = "nvim_lsp",
        --   entry_filter = function(entry, ctx)
        --     -- Dont suggest Text from nvm_lsp
        --     return require('cmp.types').lsp.CompletionItemKind[entry:get_kind()] ~= 'Text'
        --   end },
        {
          name = 'buffer',
          option = {
            keyword_length = 2,
            get_bufnrs = get_bufnrs_to_complete_from,
          },
          sorting = {
            comparators = {
              function(...) return require'cmp_buffer':compare_locality(...) end,
            },
          }
        },

        -- { name = 'vsnip' },
        -- { name = 'path' },
        { name = 'nvim_lsp_signature_help' },
        -- { name = 'vim-dadbod-completion' },

        {
          name = 'spell',
          option = {
            keep_all_entries = false,
            enable_in_context = function()
              -- Could also only enable in prose FTs...
              return vim.opt.spell:get()
            end,
          },
        },

        -- sorting = {
        --   comparators = {
        --     function(...) return cmp_buffer:compare_locality(...) end,
        --   }
        -- }
      }
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources or {}, sources))

      -- Setting up mappings for supertab-like behaviour
      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end

      local luasnip = require("luasnip")
      opts.mapping = cmp.mapping.preset.insert({
        ["<C-n>"] = cmp.mapping.select_next_item(),
        ["<C-p>"] = cmp.mapping.select_prev_item(),
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-e>"] = cmp.mapping(function(fallback)
          if luasnip.expand_or_locally_jumpable() then
            luasnip.expand_or_jump()
          else
            cmp.confirm({
              behavior = cmp.ConfirmBehavior.Insert,
              select = true,
            })
            -- fallback()
          end
        end, { "i", "s" }),
        ["<Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { "i", "s" }),
        ["<S-CR>"] = cmp.mapping.confirm({
          behavior = cmp.ConfirmBehavior.Replace,
          select = true,
        }),
        -- Because I tab and go, the current selection when I push space is already the one I want.
        -- But this _is_ useful for snippets, because they will be expanded, not just selected.
        -- 24/09/23: disabled because it breaks abbreviations.
        -- See https://github.com/hrsh7th/nvim-cmp/issues/103
        -- ['<space>'] = function(fallback)
        --   if cmp.visible() and cmp.expandable then
        --     cmp.mapping.confirm({ select = false })
        --     vim.fn.feedkeys(' ')
        --   else
        --     fallback()
        --   end
        -- end,
      })

    end,

    -- config=function ()
      -- require'cmp-spell'
      -- require'cmp_nvim_lsp'
      -- cmp.setup.cmdline(':', {
      --   sources = cmp.config.sources({
      --     { name = 'path' },
      --     { name = 'cmdline' }
      --   })
      -- })

      -- cmp.setup.cmdline('/', {
      --   sources = cmp.config.sources({
      --     { name = 'buffer' },
      --     { name = 'nvim_lsp_document_symbol' }
      --   })
      -- })
      -- cmp.setup.filetype("cmdpalette", {
      --    mapping = cmp.mapping.preset.cmdline(),
      --    sources = {
      --      { name = "cmdline" },
      --    },
      --  })

--       -- Load vsnip when file opens, to avoid lag on insert mode.
--       -- Will see whether this slows down opening a file though...
--       vim.api.nvim_exec([[
-- autocmd InsertEnter * call vsnip#get_complete_items(bufnr())
-- ]], false)

    -- end,
    -- init = function()
      -- -- https://github.com/ray-x/lsp_signature.nvim
      -- vim.o.completeopt = "menu,menuone,noselect,noinsert"

      -- Debounce tabstop syncing.
      -- vim.g.vsnip_sync_delay = 20
      -- vim.g.vsnip_choice_delay = 250
    -- end,


    dependencies = {
      {'hrsh7th/cmp-nvim-lsp'},
      {'hrsh7th/cmp-buffer'},
      {'hrsh7th/cmp-path'},
      -- {'hrsh7th/cmp-vsnip', lazy=true},
      {'hrsh7th/cmp-nvim-lsp-signature-help'},
      {'hrsh7th/cmp-nvim-lsp-document-symbol'},
      {'https://github.com/f3fora/cmp-spell'},
    }
  },

  -- -- Tab and S-Tab keys need to be mapped to <C-n> and <C-p> when completion menu is visible. Following example will use Tab and S-Tab (shift+tab) to navigate completion menu and jump between vim-vsnip placeholders when possible:

  -- local t = function(str)
  --   return vim.api.nvim_replace_termcodes(str, true, true, true)
  -- end

  -- local check_back_space = function()
  --     local col = vim.fn.col('.') - 1
  --     return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
  -- end

  -- -- Use (s-)tab to:
  -- --- move to prev/next item in completion menuone
  -- --- jump to prev/next snippet's placeholder
  -- _G.tab_complete = function()
  --   if vim.fn.pumvisible() == 1 then
  --     return t "<C-n>"
  --   elseif vim.fn['vsnip#available'](1) == 1 then
  --     return t "<Plug>(vsnip-expand-or-jump)"
  --   elseif check_back_space() then
  --     return t "<Tab>"
  --   else
  --     return vim.fn['cmp#complete']()
  --   end
  -- end
  -- _G.s_tab_complete = function()
  --   if vim.fn.pumvisible() == 1 then
  --     return t "<C-p>"
  --   elseif vim.fn['vsnip#jumpable'](-1) == 1 then
  --     return t "<Plug>(vsnip-jump-prev)"
  --   else
  --     -- If <S-Tab> is not working in your terminal, change it to <C-h>
  --     return t "<S-Tab>"
  --   end
  -- end

  -- vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
  -- vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
  -- vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
  -- vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

}
