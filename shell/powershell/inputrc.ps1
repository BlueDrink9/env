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
function mapTwoLetterNormal($a, $b){
  mapTwoLetterFunc $a $b -func $function:setViCommandMode
}
function setViCommandMode{
    [Microsoft.PowerShell.PSConsoleReadLine]::ViCommandMode()
}

function replaceWithExit {
    [Microsoft.PowerShell.PSConsoleReadLine]::BackwardKillLine()
    [Microsoft.PowerShell.PSConsoleReadLine]::KillLine()
    [Microsoft.PowerShell.PSConsoleReadLine]::Insert('exit')
}
Set-PSReadLineKeyHandler -Chord ";" -ScriptBlock { mapTwoLetterFunc ';' 'q' -func $function:replaceWithExit }

function mapTwoLetterFunc($a,$b,$func) {
  if ([Microsoft.PowerShell.PSConsoleReadLine]::InViInsertMode()) {
    $key = $host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
    if ($key.Character -eq $b) {
        &$func
    } else {
      [Microsoft.Powershell.PSConsoleReadLine]::Insert("$a")
      $wshell = New-Object -ComObject wscript.shell
      $wshell.SendKeys("{$($key.Character)}")
    }
  }
}

# Set-PSReadLineKeyHandler -Chord `;,q -Function HistorySearchBackward

Set-PSReadlineKeyHandler -Key UpArrow -Function HistorySearchBackward
Set-PSReadlineKeyHandler -Key DownArrow -Function HistorySearchForward

# Complete to LCS?
Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete
# Set-PSReadlineKeyHandler -Key Shift+Tab -Function TabCompletePrevious

# Menu that pops up when completing
Set-PSReadlineOption -ShowToolTips
Set-PSReadlineOption -CompletionQueryItems 100
Set-PSReadlineOption -BellStyle None
# Set-PSDebug -Trace 0

# Don't colour command name yellow!
Set-PSReadLineOption -Colors @{ "CommandColor"="`e[0x1b;37m" }

