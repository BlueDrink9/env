# Check out https://github.com/cspotcode/PS-GuiCompletion
# Check out https://github.com/joonro/Get-ChildItem-Color
# https://www.powershellgallery.com/packages/oh-my-posh/2.0.225
# console setting tool. Easy solarized!
# https://github.com/lukesampson/concfg
# Complete to LCS?
Set-PSReadlineKeyHandler -Key Tab -Function Complete
# Alternative, by default mapped to c-space
# Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

# Menu that pops up when completing
Set-PSReadlineOption -ShowToolTips
Set-PSReadlineOption -CompletionQueryItems 100
# Cursor changes size to indicate vi mode!
Set-PSReadlineOption -EditMode vi -ViModeIndicator cursor
