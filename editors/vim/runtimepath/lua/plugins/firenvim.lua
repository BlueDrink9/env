-- plugin firenvim in chrome and firefox.
-- Open textframes in nvim, similar to wasavi.

local spec = {
    "https://github.com/glacambre/firenvim",
    lazy = not vim.g.started_by_firenvim,
    module = false,
    build = function()
        -- " if has('win32')
        -- "     let s:firenvim_startup_prologue='"set LITE_SYSTEM=1"'
        -- " else
        -- "     let s:firenvim_startup_prologue='"export LITE_SYSTEM=1"'
        -- " endif
        -- let s:firenvim_startup_prologue=''
        -- let g:firenvim_install=":call firenvim#install(0, " . s:firenvim_startup_prologue . ")"
        vim.cmd("Lazy load firenvim")
        vim.fn["firenvim#install"](0)
    end
}

if not vim.g.started_by_firenvim then return spec end

spec.config = function()

    local debugMessages = {}

    -- Configured as json, basically.
    -- disable by default. Manually activate with chrome binding.
    -- Use alt:all to always capture alt instead of sending a special key.
    -- Mainly for OSX. Use alphanum to ignore alt for alphanum.
    -- I don't use any alt mappings anyway, since terminals don't always
    -- support them.

    -- Have to set vim.g.firenvim_config to a whole dict, so create it
    -- first - don't modify vim.g.firenvim_config directly.
    local config = {
        globalSettings = {
            alt = 'alphanum',
            cmdlineTimeout = 3000,
        },
        localSettings = {
            ['.*'] = {
                priority = 0,
                selector = [[
                    textarea:not(.crayon-plain):not([aria-readonly]),
                    div[role="textbox"]:not([aria-readonly])
                    ]],
                cmdline = 'firenvim',
                takeover = 'never',
            },
        }
    }

    local disabled_sites = {
        'docs.google.com*',
        'facebook.com*',
        'deepl.com*',
        'messenger.com*',
        'twitter.com*',
        'roll20.com*',
        'habitica.com*',
        'jira.*',
        '*.music.apple.com*',
    }
    for _, site in ipairs(disabled_sites) do
        config.localSettings[site] = { priority = 1, selector = '', takeover = 'never' }
    end

    local force_sites = {
        'codewars.com*',
    }
    for _, site in ipairs(force_sites) do
        config.localSettings[site] = { takeover = 'always' }
    end


    local eval_js = function(js)
        vim.fn['firenvim#eval_js'](js)
    end
    local press_keys = function(keys)
        vim.fn['firenvim#press_keys'](keys)
    end

    local FirenvimSetGUIOptions = function()
        vim.o.showtabline=0
        vim.o.cmdheight=0
        if vim.o.lines < 2 then
            vim.cmd('quitall!')
        end
        -- call add(s:debugMessages, 'setup')
        vim.g.hasGUI = 1

        -- Tested to match github default size on arch bspwm brave.
        vim.fn.SetGFN(9)
        vim.o.termguicolors = true
        table.insert(vim.g.customHLGroups, 'EndOfBuffer guifg=guibg')
        vim.o.colorcolumn = '0'
        vim.o.background = 'light'
        vim.opt.shortmess:append "atTFoW"
        vim.cmd('colorscheme github')
    end

    vim.api.nvim_create_autocmd("UIEnter", {
        pattern="*", group="myPlugins", nested=true,
        callback=FirenvimSetGUIOptions
    })

    vim.defer_fn(vim.fn.SetGFN, 3000)

    -- Convert the Autosave function for firenvim usage
    function Autosave()
        if vim.bo.ro
            and vim.bo.modifiable
            and vim.bo.autowrite then
            vim.cmd('silent! update')
        end
    end
    vim.cmd([[
        function! Autosave()
        lua Autosave()
        endf
        ]])
    -- Autosave on idle
    vim.api.nvim_create_autocmd({'TextChanged', 'TextChangedI'}, {
        group="myPlugins",
        callback = function()
            if vim.g.autowrite_timer_started == true then
                return
            end
            vim.g.autowrite_timer_started = true
            vim.fn.timer_start(2000, function()
                vim.g.autowrite_timer_started = false
                vim.fn.Autosave()
            end)
        end
    })

    -- Fix odd bug that sometimes stops firenvim loading the text if setting a
    -- colourscheme from a plugin. Will be overridden in setup anyway.
    vim.g.colorSch = 'default'

    -- Don't bother with whitespace plugin on save
    vim.g.strip_whitespace_on_save = 0

    -- We are in firenvim
    -- For debug messages during startup. After startup, use echom.
    if #debugMessages > 0 then
        vim.cmd([[autocmd myPlugins BufWinEnter * echom table.concat(debugMessages, "\n")]])
    end


    -- Universal mappings
    vim.keymap.set('n', '<C-CR>',
        function()
            press_keys("<LT>C-CR>")
            vim.cmd.norm("ggdG:q!")
        end)
    vim.keymap.set('n', '<C-z>',
        function() vim.fn['firenvim#hide_frame']() end
    )
    vim.keymap.set('n', '<Esc><Esc><Esc>',
        function() vim.fn['firenvim#focus_page']() end
    )
    vim.keymap.set({'i', 'c'}, '<D-v>', '<c-r>+')
    vim.api.nvim_set_keymap('v', '<D-c>', '"+y', { noremap = true })

    -- Auto-enter insertmode if the buffer is empty.
    vim.cmd([[
        autocmd myPlugins BufWinEnter * if line('$') == 1 && getline(1) == ''
        \ && bufname() != '' | startinsert | endif
        ]])

    -- Use buffer mappings to ensure they override other mappings, even if
    -- unlikely to change buffers in firenvim. Also future-proofs.
    local FirenvimSetPageOptions = function()
        local bufname = vim.fn.expand('%:t')
        -- Use <LT>CR> to escape the CR and send it as a string, rather than
        -- including it in the mapping. Needed for mapping to press_keys.
        if string.find(bufname, 'github.com') then
            vim.cmd('colorscheme github')
            vim.o.ft='markdown'
            local clickSubmitButtonJS = [[
                'document.getElementById(
                "partial-new-comment-form-actions"
                ).getElementsByClassName("btn btn-primary")[0].click();'
                ]]
            vim.keymap.set({'i', 'n'}, '<C-CR>',
                function() eval_js(clickSubmitButtonJS) end, { buffer = true })
        elseif string.find(bufname, 'cocalc.com') or
            string.find(bufname, 'kaggleusercontent.com') then
            vim.o.ft='python'
        elseif string.find(bufname, 'localhost') or
            -- Assume Jupyter notebook.
            string.find(bufname, '127.0.0.1') then
            vim.o.ft='python'
        elseif string.find(bufname, 'reddit.com') then
            vim.o.ft='markdown'
        elseif string.find(bufname, 'stackexchange.com') or
            string.find(bufname, 'stackoverflow.com') then
            vim.o.ft='markdown'
        elseif string.find(bufname, 'codewars.com') then
            vim.cmd('colorscheme dracula')
            vim.keymap.set('n', '<C-CR>',
                function() press_keys("<LT>C-CR>") end,
                { buffer = true })
            vim.keymap.set('n', '<C-\'>',
                function() press_keys("<C-\'>") end,
                { buffer = true })
            vim.fn.IdeMode()
            -- Chat apps
        elseif string.find(bufname, 'slack.com') or
            string.find(bufname, 'gitter.im') or
            string.find(bufname, 'webchat.kde.org') then
            vim.o.ft='markdown'
            vim.bo.norelativenumber = true
            vim.bo.nonumber = true
            -- For chat apps. Enter sends the message and deletes the buffer.
            -- Shift enter is normal return.
            vim.keymap.set('i', '<s-CR>', '<CR>', { buffer = true })
            -- Insert mode start by default.
            vim.cmd('normal! i')
            if string.find(bufname, 'slack.com') then
                -- slack doesn't actually respond to press_keys (see firenvim readme).
                -- Requires the send button to be enabled for this workspace.
                local clickSubmitButtonJS = [[
                    'document.getElementsByClassName(
                    "c-icon c-icon--paperplane-filled")[0].click();'
                    ]]
                vim.keymap.set('i', '<CR>',
                    function()
                        eval_js(clickSubmitButtonJS)
                        vim.cmd.norm("ggdGa")
                    end,
                    { buffer = true })
            else
                vim.keymap.set('i', '<CR>',
                    function()
                        press_keys("<LT>CR>")
                        vim.cmd.norm("ggdGa")
                    end,
                    { buffer = true })
            end
        end
    end

    vim.api.nvim_create_autocmd("BufEnter", {
        pattern="*", group="myPlugins", nested=true,
        callback=FirenvimSetPageOptions}) 

    vim.g.firenvim_config = config
end

-- TODO fix lualine + lazyvim + firenvim
return {
    spec,
    {'lualine.nvim', event="VimResized",}
}
