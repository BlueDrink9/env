local maps = vim.g.IDE_mappings or {}

-- Custom operator function to mimic neoterm's paragraph/line sending
_G.snacks_repl_opfunc = function(motion_type)
  if motion_type == nil then
    vim.o.opfunc = "v:lua.snacks_repl_opfunc"
    return "g@"
  end

  local start_mark = (motion_type == 'v' or motion_type == 'V') and "'<" or "'["
  local end_mark   = (motion_type == 'v' or motion_type == 'V') and "'>" or "']"
  local lines = vim.api.nvim_buf_get_lines(0, vim.fn.line(start_mark) - 1, vim.fn.line(end_mark), false)
  local text = table.concat(lines, "\n") .. "\n"

  -- Auto-detect the correct REPL program based on filetype
  local ft = vim.bo.filetype
  local cmd_map = {
    python = "ipython",
    r      = "R",
    sh     = "bash",
    lua    = "lua"
  }
  local cmd = cmd_map[ft] or vim.o.shell

  -- Find existing terminal or spawn a new one via Snacks
  local term_buf = nil
  for _, b in ipairs(vim.api.nvim_list_bufs()) do
    if vim.bo[b].buftype == "terminal" and string.find(vim.api.nvim_buf_get_name(b) or "", cmd) then
      term_buf = b
      break
    end
  end

  if not term_buf then
    Snacks.terminal(cmd, { win = { position = "bottom", height = 15 } })
    vim.cmd("wincmd p") -- Return focus to the code buffer
    for _, b in ipairs(vim.api.nvim_list_bufs()) do
      if vim.bo[b].buftype == "terminal" then term_buf = b break end
    end
  end

  -- Send the text payload
  if term_buf then
    local chan = vim.b[term_buf].terminal_job_id
    if chan then vim.api.nvim_chan_send(chan, text) end
  end
end

return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    picker = {
      enabled = true,
      hidden = true,
    },
    terminal = {
      enabled = true,
      win = {
        position = "bottom",
        height = 10,
      },
    },
    bufdelete = { enabled = true },
    indent = { enabled = true },
    dim = { enabled = true },
    zen = { enabled = true },
    input = { enabled = true },
    notifier = { enabled = true },
    dashboard = { enabled = false },
  },
  keys = {
    -- Pickers mapping to legacy IDE_mappings
    { maps.FuzzyCommands or "<leader>;", function() Snacks.picker.commands() end, desc = "Commands" },
    { "<leader>:", function() Snacks.picker.command_history() end, desc = "Command History" },
    { maps.FuzzyOpenFile or "<leader><leader>", function() Snacks.picker.files() end, desc = "Find Files" },
    { maps.FuzzyOldFiles or "<leader><cr>r", function() Snacks.picker.recent() end, desc = "Recent Files (MRU)" },
    { maps.FuzzyBuffers or "<leader>,", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { maps.FuzzySearchBuffer or "<leader>/", function() Snacks.picker.lines() end, desc = "Search Buffer Lines" },
    { maps.FuzzySearchFiles or "<leader>??", function() Snacks.picker.grep() end, desc = "Grep Project" },
    { maps.FuzzySymbols or "<leader>j", function() Snacks.picker.lsp_symbols() end, desc = "Document Symbols" },
    { maps.FuzzySymbolsWorkspace or "<leader>J", function() Snacks.picker.lsp_workspace_symbols() end, desc = "Workspace Symbols" },

    -- Visual Undo (replaces vim-mundo)
    { "<leader>u", function() Snacks.picker.undo() end, desc = "Undo History Tree" },

    -- Terminal Toggle
    { "<C-s>", function() Snacks.terminal.toggle() end, desc = "Toggle Terminal", mode = { "n", "t" } },

    -- Layout-preserving Buffer Delete (replaces vim-bbye :Bdelete)
    { "<C-w>x", function() Snacks.bufdelete() end, desc = "Delete Buffer (Preserve Splits)", mode = "n" },
    
    -- Window Zoom (replaces zoomwintab and goyo)
    { "<C-w>z", function() Snacks.zen.zoom() end, desc = "Zoom Split Window", mode = "n" },

    -- Custom REPL Sender via Opfunc (handles visual, line, and motions like `yip`)
    { maps.REPLSend or "<leader>sr", _G.snacks_repl_opfunc, mode = { "n", "x" }, expr = true, desc = "Send Motion/Selection to Auto-REPL" },
    { (maps.REPLSend or "<leader>sr") .. (maps.REPLSend or "<leader>sr"):sub(-1), function() return _G.snacks_repl_opfunc() .. "_" end, mode = "n", expr = true, desc = "Send Line to Auto-REPL" },
  },
  init = function()
    -- Compatibility user command for :Bdelete
    vim.api.nvim_create_user_command("Bdelete", function()
      Snacks.bufdelete()
    end, {})
  end,
}
