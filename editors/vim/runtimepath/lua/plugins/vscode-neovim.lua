-- vim: foldmethod=marker
-- vim: foldmarker={[},{]}

local spec = {
    "vscode-neovim/vscode-neovim",
    lazy = vim.g.vscode ~= 1,
}

if vim.g.vscode ~= 1 then return spec end

local idemaps = vim.g.IDE_mappings
-- {[} Mappings

local function VSCodeMapDict(mappings, visualMappings)
    local vscode = require('vscode')
    for key, value in pairs(mappings) do
        vim.keymap.set('n', key,
            function() vscode.call(value) end)
    end
    for key, value in pairs(visualMappings) do
        vim.keymap.set('v', key,
            function() vscode.call(value) end)
    end
end

-- Not going to work because vscode-neovim doesn't do imaps
-- inoremap kv :call VSCodeCall("vscode-neovim.escape")
-- inoremap vk :call VSCodeCall("vscode-neovim.escape")

local function VSCodeMaps()
    local nmappings = {
        [idemaps.definition] = 'editor.action.goToDeclaration',
        [idemaps.definition2] = 'editor.action.peakDefinition',
        [idemaps.implementation] = 'editor.gotoImplementation',
        [idemaps.implementation2] = 'editor.gotoImplementation',
        [idemaps.typeDefinition] = 'editor.action.goToTypeDefinition',
        [idemaps.hover] = 'editor.action.showHover',
        [idemaps.refactor] = 'editor.action.refactor',
        [idemaps.rename] = 'editor.action.rename',
        [idemaps.diagnostic] = 'editor.action.showHover',
        [idemaps.fix] = 'editor.action.autoFix',
        [idemaps.codeAction] = 'editor.action.quickFix',
        [idemaps.signature] = 'editor.action.triggerParameterHints',
        [idemaps.references] = 'editor.action.findReferences',
        [idemaps.references2] = 'editor.action.findReferences',
        [idemaps.FuzzyOpenFile] = 'workbench.action.quickOpen',
        [idemaps.FuzzyBuffers] = 'workbench.action.quickOpen',
        [idemaps.FuzzyTags] = 'workbench.action.showAllSymbols',
        [idemaps.FuzzyCommands] = 'workbench.action.showCommands',
        [idemaps.FuzzyOldFiles] = 'workbench.action.openRecent',
        [idemaps.GitCommit] = 'git.commit',
        [idemaps.GitStage] = 'git.stage',
        [idemaps.GitAmend] = 'git.commitStagedAmend',
        -- [idemaps.debugFile] = 'workbench.action.debug.run',
        -- Restart will start if not runninng, same as debug.run.
        -- Meanwhile debug.run will start new instanec rather than
        -- continue. Prefer to keep that mapping for continue...
        [idemaps.debugFile] = 'workbench.action.debug.continue',
        [idemaps.debugStart] = 'workbench.action.debug.run',
        [idemaps.debugRestart] = 'workbench.action.debug.restart',
        [idemaps.debugReset] = 'workbench.action.debug.stop',
        [idemaps.debugContinue] = 'workbench.action.debug.continue',
        [idemaps.debugPause] = 'workbench.action.debug.pause',
        [idemaps.debugStepOver] = 'workbench.action.debug.stepOver',
        [idemaps.debugStepInto] = 'workbench.action.debug.stepInto',
        [idemaps.debugStepOut] = 'workbench.action.debug.stepOut',
        [idemaps.debugHover] = 'workbench.action.debug.showDebugHover',
        [idemaps.setBreakpoint] = 'editor.debug.action.toggleInlineBreakpoint',
        [idemaps.clearBreakpoints] = 'editor.clearBreakpoints',
        [idemaps.addBreakpointFunctional] = 'workbench.debug.viewlet.action.addFunctionBreakpointAction',
        [idemaps.setBreakpointConditional] = 'editor.debug.action.conditionalBreakpoint',
        [idemaps.make] = 'workbench.action.tasks.build',
        [idemaps.diagnosticNext] = 'editor.action.marker.next',
        [idemaps.diagnosticPrev] = 'editor.action.marker.prev',
        ['<c-s>'] = 'workbench.action.terminal.toggleTerminal',
        ['<leader>a'] = 'workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup'
        -- [idemaps.definition] = 'editor.action.revealDefinition',
        -- [idemaps.type_definition] = 'editor.action.revealDeclaration',
        -- [idemaps.fix] = 'editor.action.quickFix',
    }

    -- 'tab' is just to sync the visual selection with vscode.
    local vmappings = {
        ['<tab>'] = '""',
        [idemaps.FuzzyCommands] = 'workbench.action.showCommands',
        [idemaps.GitStage] = 'git.stageSelectedRanges',
        [idemaps.GitUnstage] = 'git.unstageSelectedRanges',
        [idemaps.debugConsoleSend] = 'editor.debug.action.selectionToRepl',
    }

    VSCodeMapDict(nmappings, vmappings)
end

-- Window management
vim.keymap.set('n', '<leader>w', '<C-w>v<C-w>l')
vim.keymap.set('n', '<C-w>t', ':tab sb<cr>')

VSCodeMapDict({
    ['<C-h>'] = 'workbench.action.focusLeftGroup',
    ['<C-l>'] = 'workbench.action.focusRightGroup',
    ['<C-k>'] = 'workbench.action.focusAboveGroup',
    ['<C-j>'] = 'workbench.action.focusBelowGroup'
}, {})

-- Open windows to the left, right, up, down, like in tmux
VSCodeMapDict({
    ['<C-w>h'] = 'workbench.action.splitEditorLeft',
    ['<C-w>l'] = 'workbench.action.splitEditorRight',
    ['<C-w>k'] = 'workbench.action.splitEditorUp',
    ['<C-w>j'] = 'workbench.action.splitEditorDown'
}, {})

-- Cycle through buffers
VSCodeMapDict({
    ['<Right>'] = 'workbench.action.nextEditor',
    ['<Left>'] = 'workbench.action.previousEditor'
}, {})

-- Resize windows
vim.keymap.set('n', '<S-Right>', '5<C-W>>')
vim.keymap.set('n', '<S-Left>', '5<C-W><')
vim.keymap.set('n', '<S-Up>', '3<C-W>+')
vim.keymap.set('n', '<S-Down>', '3<C-W>-')

-- Zoom window into its own tab
vim.keymap.set('n', '<C-w>z', ':tab split<CR>', { silent = true })

-- Kill current buffer
vim.keymap.set('n', '<C-w>x', ':bdel<tab><CR>', { silent = true })

vim.keymap.set('x', '<leader>c', '<Plug>VSCodeCommentary', {remap=false})
vim.keymap.set('n', '<leader>c', '<Plug>VSCodeCommentary', {remap=false})
vim.keymap.set('o', '<leader>c', '<Plug>VSCodeCommentary', {remap=false})
vim.keymap.set('n', '<leader>cc', '<Plug>VSCodeCommentaryLine', {remap=false})

-- Undo my \v mapping
vim.keymap.set('n', '/', '/')
vim.keymap.set('n', 'n', 'n')
vim.keymap.set('n', 'N', 'N')

vim.opt.number = false
vim.opt.relativenumber = false
vim.opt.colorcolumn = ''
vim.opt.autowrite = false

-- Apply mappings when mapping overrides event is triggered
vim.api.nvim_create_autocmd('User', {
    pattern = 'MappingOverrides',
    callback = VSCodeMaps
})

-- FT specific
local function VSCodeFTMaps(ft)
    if ft == 'sql' then
        REPLSendCmd = 'mssql.runQuery'
        VSCodeMapDict({
            [idemaps.REPLSendFile] = REPLSendCmd,
            [idemaps.REPLCancel] = 'mssql.cancelQuery',
        }, {
            [idemaps.REPLSend] = 'mssql.runQuery'
        })
    elseif ft == 'python' then
        REPLSendCmd = 'jupyter.execSelectionInteractive'
        VSCodeMapDict({
            [idemaps.REPLSendFile] = 'jupyter.runFileInteractive',
            [idemaps.runFile] = 'python.execInTerminal',
        }, {
            [idemaps.REPLSend] = REPLSendCmd
        })
    else
        -- No filetype specific mappings for this ft yet
        return
    end
        VSCodeMapDict({
            [idemaps.REPLSendFile] = 'jupyter.runFileInteractive'
        }, {
            [idemaps.REPLSend] = REPLSendCmd
        })

    vim.keymap.set('v', idemaps.REPLSend,
        function()
            -- Reset visual mode so the selection is passed over
            vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes('<esc>', true, false, true), 'x', true)
            require('vscode').action(REPLSendCmd, {range = {vim.fn.line("'<"), vim.fn.line("'>")}})
        end
    )

    _G.opfuncRunMotion = function(type)
        if type == nil or type == '' then
            vim.opt.opfunc = 'v:lua.opfuncRunMotion'
            return 'g@'
        end
        if not REPLSendCmd then
            print("no repl command set for this filetype: '" .. vim.bo.filetype .. "'")
            return
        end
        local line1, line2 = vim.fn.line("'["), vim.fn.line("']")
        -- 0 to deselect visual after calling. Call rather than notify to
        -- not deselect until after.
        require('vscode').action(REPLSendCmd, { range = range })
    end

    vim.keymap.set('n', idemaps.REPLSend, opfuncRunMotion, { expr = true })
    local idemaps = vim.g.IDE_mappings
    vim.keymap.set('n', idemaps.REPLSendLine, function()
        return opfuncRunMotion() .. '_'
    end, { expr = true })
end


-- Using vscode api to map operator
local function esc()
  local key = vim.api.nvim_replace_termcodes("<esc>", true, true, true)
  vim.api.nvim_feedkeys(key, "n", false)
end
local debugConsoleSend = vscode.to_op(function(ctx)
    vscode.action("editor.debug.action.selectionToRepl", { range = ctx.range, callback = esc })
end)
local debugConsoleSendLine = function()
    return debugConsoleSend() .. "_"
end

vim.keymap.set({ "n", "x" }, idemaps.debugConsoleSend, debugConsoleSend)
vim.keymap.set({ "n", "x" }, idemaps.debugConsoleSendLine, debugConsoleSendLine)

-- local comment = vscode.to_op(function(ctx)
--   local cmd = ctx.is_linewise and "editor.action.commentLine" or "editor.action.blockComment"
--   local opts = { range = ctx.range, callback = esc }
--   if ctx.is_linewise and ctx.is_current_line then
--     opts.range = nil
--   end
--   vscode.action(cmd, opts)
-- end

vim.api.nvim_create_autocmd('FileType', {
    pattern = {'sql', 'python'},
    callback = function()
        VSCodeFTMaps(vim.bo.filetype)
    end
})

return spec
