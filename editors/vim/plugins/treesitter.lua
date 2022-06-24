function check_treesitter_installable()
    -- ("tar" and "curl" or "git") and {
    local fn = vim.fn
    if fn.executable("git") == 0 then
        if fn.executable("curl") == 0 and fn.executable("tar") == 0 then
            return false
        end
    end
    CCompilers = { vim.fn.getenv("CC"), "cc", "gcc", "clang", "cl", "zig" }
    CCompilers = { "cc", "gcc", "clang", "cl", "zig" }
    cc = false
    for _, compiler in pairs(CCompilers) do
        if fn.executable(compiler) == 1 then
            cc = true
            break
        end
    end
    if not cc then
        return false
    end
    return true
end

if not check_treesitter_installable() then
    return
end


vim.cmd("Plug 'https://github.com/nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}")
vim.cmd("Plug 'nvim-treesitter/playground'")

vim.cmd("Plug 'https://github.com/ThePrimeagen/refactoring.nvim'")
vim.cmd("autocmd myPlugins User pluginSettingsToExec lua refactoring_nvim_setup()")
function refactoring_nvim_setup()
    require('refactoring').setup({})
    local maps = vim.api.nvim_get_var('IDE_mappings')
    -- prompt for a refactor to apply when the remap is triggered
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
vim.cmd("Plug 'https://github.com/danymat/neogen'")
vim.cmd("autocmd myPlugins User pluginSettingsToExec lua require('neogen').setup {}")
vim.cmd("command! Annotate lua require('neogen').generate()")

vim.cmd("Plug 'https://github.com/RRethy/nvim-treesitter-endwise'")
vim.cmd("autocmd myPlugins User pluginSettingsToExec lua require('nvim-treesitter.configs').setup { endwise = { enable = true, }, }")
