-- This is here to prevent vim detecting modelines
--
--
--
--

local VimMode = hs.loadSpoon('VimMode')
local vim = VimMode:new()
-- vim:enterWithSequence('kv')
vim:bindHotKeys({ enter = {{'ctrl'}, '['} })

-- Check activity monitor for app names.
vim
    :disableForApp('Code')
    :disableForApp('iTerm')
    :disableForApp('kitty')
    :disableForApp('MacVim')
    :disableForApp('Terminal')
    :disableForApp('nvim')

-- Show alert when entering normal mode
vim:shouldShowAlertInNormalMode(true)
vim:shouldDimScreenInNormalMode(false)

-- This is here to prevent vim detecting modelines
--
--
--
--
