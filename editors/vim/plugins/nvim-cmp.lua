-- https://github.com/ray-x/lsp_signature.nvim
vim.o.completeopt = "menuone,noselect"
local cmp = require'cmp'
local cmp_buffer = require('cmp_buffer')
Mappings = cmp.mapping

Get_bufnrs_to_complete_from = function()
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

cmp.setup({
  --   completion = {
  --       autocomplete = true,
  --   },
  -- enabled = true;
  -- autocomplete = true;
  -- debug = false;
  -- min_length = 1;
  preselect = cmp.PreselectMode.None;
  -- throttle_time = 80;
  -- source_timeout = 200;
  -- resolve_timeout = 800;
  -- incomplete_delay = 400;
  -- max_abbr_width = 100;
  -- max_kind_width = 100;
  -- max_menu_width = 100;
  -- documentation = {
  --   border = { '', '' ,'', ' ', '', '', '', ' ' }, -- the border option is the same as `|help nvim_open_win|`
  --   winhighlight = "NormalFloat:cmpDocumentation,FloatBorder:CompeDocumentationBorder",
  --   max_width = 120,
  --   min_width = 60,
  --   max_height = math.floor(vim.o.lines * 0.3),
  --   min_height = 1,
  -- },
  -- view = {
  --   entries = {name = 'custom', selection_order = 'near_cursor' }
  -- },
  performance = {
    debounce = 200,
    throttle = 20,
  },

  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
      -- require('luasnip').lsp_expand(args.body)
    end,
  },

  sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        {
           name = 'buffer',
           option = {
              keyword_length = 2,
              get_bufnrs = Get_bufnrs_to_complete_from,
           },
        },
        { name = 'vsnip' },
        { name = 'path' },
        { name = 'cmdline' },
        { name = 'nvim_lsp_signature_help' },
        { name = 'vim-dadbod-completion' },
        {
           name = 'spell',
           option = {
              keep_all_entries = false,
              enable_in_context = function()
                 return true
              end,
           },
        },
        sorting = {
           comparators = {
              function(...) return cmp_buffer:compare_locality(...) end,
           }
        }
     }),

  mapping = {
      ['<C-e>'] = Mappings(Mappings.complete(), { 'i', 'c' }),
      ['<C-e>'] = Mappings.confirm({ select = false }),
      -- Because I tab and go, the current selection when I push space is already the one I want. But this _is_ useful for snippets, because they will be expanded, not just selected.
      ['<space>'] = Mappings.confirm({ select = false }),
      ['<return>'] = Mappings.confirm({ select = false }),
      ["<Tab>"] = Mappings(Mappings.select_next_item({behavior=cmp.SelectBehavior.Insert}), { 'i', 'c' }),
      ["<S-Tab>"] = Mappings(Mappings.select_prev_item({behavior=cmp.SelectBehavior.Insert}), { 'i', 'c' }),
      ['<C-b>'] = Mappings(Mappings.scroll_docs(-4), { 'i' }),
      ['<C-f>'] = Mappings(Mappings.scroll_docs(4), { 'i' }),
  },

})

cmp.setup.cmdline(':', {
    sources = cmp.config.sources({
        { name = 'path' },
        { name = 'cmdline' }
    })
})

cmp.setup.cmdline('/', {
    sources = cmp.config.sources({
       { name = 'buffer' },
       { name = 'nvim_lsp_document_symbol' }
    })
})


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
