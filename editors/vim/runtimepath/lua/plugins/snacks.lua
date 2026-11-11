local maps = vim.g.IDE_mappings or {}

-- 1. Helper to route text to an active or newly spawned REPL terminal
local function send_to_repl(text)
  local ft = vim.bo.filetype
  local cmd_map = {
    python = "ipython",
    r      = "R",
    sh     = "bash",
    lua    = "lua",
  }
  local cmd = cmd_map[ft] or vim.o.shell

  -- Find existing terminal buffer matching the REPL command
  local term_buf = nil
  for _, b in ipairs(vim.api.nvim_list_bufs()) do
    if vim.bo[b].buftype == "terminal" and string.find(vim.api.nvim_buf_get_name(b) or "", cmd) then
      term_buf = b
      break
    end
  end

  -- Spawn terminal if not found
  if not term_buf then
    Snacks.terminal(cmd, { win = { position = "bottom", height = 15 } })
    vim.cmd("wincmd p") -- Keep focus in the source buffer
    for _, b in ipairs(vim.api.nvim_list_bufs()) do
      if vim.bo[b].buftype == "terminal" then
        term_buf = b
        break
      end
    end
  end

  -- Send text payload to terminal job channel
  if term_buf then
    local chan = vim.b[term_buf].terminal_job_id
    if chan then
      vim.api.nvim_chan_send(chan, text)
    end
  end
end

-- 2. Custom operator function mimicking Neoterm's paragraph/motion/line evaluation
_G.snacks_repl_opfunc = function(motion_type)
  if motion_type == nil then
    vim.o.opfunc = "v:lua.snacks_repl_opfunc"
    return "g@"
  end

  local start_mark = (motion_type == "v" or motion_type == "V") and "'<" or "'["
  local end_mark   = (motion_type == "v" or motion_type == "V") and "'>" or "']"
  local lines = vim.api.nvim_buf_get_lines(0, vim.fn.line(start_mark) - 1, vim.fn.line(end_mark), false)
  local text = table.concat(lines, "\n") .. "\n"

  send_to_repl(text)
end

-- 3. Dynamic High-Contrast Highlights for Snacks Picker
local function setup_snacks_highlights()
  local links = {
    SnacksPicker                   = "NormalFloat",
    SnacksPickerBorder             = "FloatBorder",
    SnacksPickerPrompt             = "Title",
    SnacksPickerMatch              = "CurSearch",
    SnacksPickerDir                = "Directory",
    SnacksPickerPathHidden         = "Comment",
    SnacksPickerGitStatusUntracked = "Special",
    SnacksPickerSelected           = "Visual",
    SnacksPickerList               = "NormalFloat",
    SnacksPickerListCursorLine     = "CursorLine",
  }
  for group, target in pairs(links) do
    vim.api.nvim_set_hl(0, group, { link = target, default = false })
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
      layout = {
        backdrop = false,
      },
      win = {
        input = {
          wo = {
            virtualedit = "all",
          },
        },
      },
    },
    terminal = {
      enabled = true,
      win = {
        position = "bottom",
        height = 10,
      },
    },
    bufdelete = { enabled = true },
    indent    = { enabled = true },
    dim       = { enabled = true },
    zen       = { enabled = true },
    input     = { enabled = true },
    notifier  = { enabled = false }, -- Managed by Noice / nvim-notify
    dashboard = { enabled = false },
  },
  keys = {
    -- 1. Core Pickers & Navigation
    { maps.FuzzyFuzzy, function() Snacks.picker() end, desc = "All Pickers" },
    { maps.FuzzyOpenFile, function() Snacks.picker.files() end, desc = "Find Files" },
    { maps.FuzzyOldFiles, function() Snacks.picker.recent() end, desc = "Recent Files (MRU)" },
    { maps.FuzzyBuffers, function() Snacks.picker.buffers() end, desc = "Buffers" },
    { maps.FuzzyCommands, function() Snacks.picker.commands() end, desc = "Commands" },
    { "<leader>:", function() Snacks.picker.command_history() end, desc = "Command History" },

    -- 2. Search Escalation Hierarchy (? family)
    { maps.FuzzySearchBuffer, function() Snacks.picker.lines() end, desc = "Search Current Buffer (BLines)" },
    { maps.FuzzySearchBuffers, function() Snacks.picker.grep_buffers() end, desc = "Search All Open Buffers (Lines)" },
    { maps.FuzzySearchFiles, function() Snacks.picker.grep() end, desc = "Grep Entire Project" },

    -- 3. Code Symbols & Tags
    { maps.FuzzySymbols, function() Snacks.picker.lsp_symbols() end, desc = "LSP Document Symbols" },
    { maps.FuzzySymbolsWorkspace, function() Snacks.picker.lsp_workspace_symbols() end, desc = "LSP Workspace Symbols" },
    { maps.FuzzyTags, function() Snacks.picker.tags() end, desc = "Tags" },

    -- 4. Extensions Parity (Undo tree, marks, projects)
    { (maps.FuzzyFuzzy) .. "u", function() Snacks.picker.undo() end, desc = "Undo History Tree" },
    { "<leader>u", function() Snacks.picker.undo() end, desc = "Undo History Tree" },
    { (maps.FuzzyFuzzy) .. "m", function() Snacks.picker.marks() end, desc = "Marks" },
    { (maps.FuzzyFuzzy) .. "r", function() Snacks.picker.projects() end, desc = "Projects / Repos" },

    -- 5. Terminal & Buffer Lifecycle
    { maps.TermToggle, function() Snacks.terminal.toggle() end, desc = "Toggle Terminal", mode = { "n", "t" } },
    { "<C-w>x", function() Snacks.bufdelete() end, desc = "Delete Buffer (Preserve Splits)", mode = "n" },
    { "<C-w>z", function() Snacks.zen.zoom() end, desc = "Zoom Split Window", mode = "n" },

    -- 6. REPL Operator & Send Parity (Replacing Neoterm)
    {
      maps.REPLSend,
      function() return _G.snacks_repl_opfunc() end,
      expr = true,
      desc = "REPL Send Operator",
    },
    {
      maps.REPLSend,
      function()
        -- Exit visual mode to set marks '< and '> before dispatching
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "x", false)
        vim.schedule(function() _G.snacks_repl_opfunc("v") end)
      end,
      mode = "x",
      desc = "REPL Send Selection",
    },
    {
      maps.REPLSendLine,
      function() return _G.snacks_repl_opfunc() .. "_" end,
      expr = true,
      desc = "REPL Send Line",
    },
    {
      maps.REPLSendFile,
      function()
        local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
        send_to_repl(table.concat(lines, "\n") .. "\n")
      end,
      desc = "REPL Send Entire Buffer",
    },
    {
      maps.REPLToggle,
      function() Snacks.terminal.toggle() end,
      desc = "Toggle REPL Window",
    },
  },
  init = function()
    -- Compatibility user command for :Bdelete
    vim.api.nvim_create_user_command("Bdelete", function()
      Snacks.bufdelete()
    end, {})

    -- Register highlight link hooks
    setup_snacks_highlights()
    vim.api.nvim_create_autocmd("ColorScheme", {
      group = vim.api.nvim_create_augroup("SnacksCustomHighlights", { clear = true }),
      callback = setup_snacks_highlights,
    })
  end,
}
