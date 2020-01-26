# Cursor changes size to indicate vi mode!
Set-PSReadlineOption -EditMode vi -ViModeIndicator cursor
function OnViModeChangeSetCursor {
    if ($args[0] -eq 'Command') {
        # Set the cursor to a blinking block.
        Write-Host -NoNewLine "`e[1 q"
    } else {
        # Set the cursor to a blinking line.
        Write-Host -NoNewLine "`e[5 q"
    }
}
if (!($PSVersionTable.PSVersion.Major -lt 7)) {
  Set-PSReadLineOption -ViModeIndicator Script -ViModeChangeHandler $Function:OnViModeChangeSetCursor
}


Set-PSReadLineKeyHandler -vimode insert -Chord "k" -ScriptBlock { mapTwoLetterNormal 'k' 'v' }
Set-PSReadLineKeyHandler -vimode insert -Chord "v" -ScriptBlock { mapTwoLetterNormal 'v' 'k' }
function mapTwoLetterNormal{
  param($a, $b)
  if ([Microsoft.PowerShell.PSConsoleReadLine]::InViInsertMode()) {
    $key = $host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
    if ($key.Character -eq $b) {
      [Microsoft.PowerShell.PSConsoleReadLine]::ViCommandMode()
    } else {
      [Microsoft.Powershell.PSConsoleReadLine]::Insert("$a")
      [Microsoft.Powershell.PSConsoleReadLine]::Insert($key.Character)
    }
  }
}

# Set-PSReadLineKeyHandler -Chord `;,q -Function HistorySearchBackward

Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward

# Complete to LCS?
# Set-PSReadlineKeyHandler -Key Tab -ScriptBlock {
#       [Microsoft.PowerShell.PSConsoleReadLine]::Insert('exit')
# }
# Alternative, by default mapped to c-space
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

# Menu that pops up when completing
Set-PSReadlineOption -ShowToolTips
Set-PSReadlineOption -CompletionQueryItems 100
Set-PSReadlineOption -BellStyle None
