# Check out https://github.com/cspotcode/PS-GuiCompletion
# Complete to LCS?
Set-PSReadlineKeyHandler -Key Tab -Function Complete
# Alternative, by default mapped to c-space
# Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

# Menu that pops up when completing
Set-PSReadlineOption -ShowToolTips
Set-PSReadlineOption -CompletionQueryItems 100
# Cursor changes size to indicate vi mode!
Set-PSReadlineOption -EditMode vi -ViModeIndicator cursor
