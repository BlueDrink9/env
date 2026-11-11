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
    -- 1. Core Pickers (Exact IDE_mappings parity)
    { maps.FuzzyFuzzy, function() Snacks.picker() end, desc = "All Pickers" },
    { maps.FuzzyOpenFile, function() Snacks.picker.files() end, desc = "Find Files" },
    { maps.FuzzyOldFiles, function() Snacks.picker.recent() end, desc = "Recent Files (MRU)" },
    { maps.FuzzyBuffers, function() Snacks.picker.buffers() end, desc = "Buffers" },
    { maps.FuzzyCommands, function() Snacks.picker.commands() end, desc = "Commands" },
    { "<leader>:", function() Snacks.picker.command_history() end, desc = "Command History" },

    -- 2. Buffer & Line Text Searching (The 3 Parity Levels)
    { maps.FuzzySearchBuffer, function() Snacks.picker.lines() end, desc = "Search Current Buffer (BLines)" },
    { maps.FuzzySearchBuffers, function() Snacks.picker.grep_buffers() end, desc = "Search All Open Buffers (Lines)" },
    { maps.FuzzySearchFiles, function() Snacks.picker.grep() end, desc = "Grep Entire Project" },

    -- 3. Code Symbols & Tags
    { maps.FuzzySymbols, function() Snacks.picker.lsp_symbols() end, desc = "LSP Document Symbols" },
    { maps.FuzzySymbolsWorkspace, function() Snacks.picker.lsp_workspace_symbols() end, desc = "LSP Workspace Symbols" },
    { maps.FuzzyTags, function() Snacks.picker.tags() end, desc = "Tags" },

    -- 4. Extensions Parity (Telescope-undo, marks, projects)
    { (maps.FuzzyFuzzy) .. "u", function() Snacks.picker.undo() end, desc = "Undo History Tree" },
    { "<leader>u", function() Snacks.picker.undo() end, desc = "Undo History Tree" },
    { (maps.FuzzyFuzzy) .. "m", function() Snacks.picker.marks() end, desc = "Marks" },
    { (maps.FuzzyFuzzy) .. "r", function() Snacks.picker.projects() end, desc = "Projects / Repos" },

    -- 5. Terminal & Buffer Lifecycle
    { "<C-s>", function() Snacks.terminal.toggle() end, desc = "Toggle Terminal", mode = { "n", "t" } },
    { "<C-w>x", function() Snacks.bufdelete() end, desc = "Delete Buffer (Preserve Splits)", mode = "n" },
    { "<C-w>z", function() Snacks.zen.zoom() end, desc = "Zoom Split Window", mode = "n" },

    -- 6. Visual REPL Send
    {
      "<leader>sr",
      function()
        local text = table.concat(require("snacks.utils").get_vsel_text(), "\n") .. "\n"
        for _, win in ipairs(vim.api.nvim_list_wins()) do
          local buf = vim.api.nvim_win_get_buf(win)
          if vim.bo[buf].buftype == "terminal" then
            local chan = vim.b[buf].terminal_job_id
            if chan then
              vim.api.nvim_chan_send(chan, text)
              vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "n", true)
              return
            end
          end
        end
        vim.notify("No active terminal found. Press <C-s> to open a terminal first.", vim.log.levels.WARN)
      end,
      mode = "x",
      desc = "Send Selection to Snacks Terminal",
    },
  },
  init = function()
    -- Compatibility user command for :Bdelete
    vim.api.nvim_create_user_command("Bdelete", function()
      Snacks.bufdelete()
    end, {})
  end,
}
