Set-PSReadlineOption -EditMode vi
# Cursor changes size to indicate vi mode!
function OnViModeChangeSetCursor {
    Param($mode)
    $Esc=[char]0x1b
    if ($mode -eq 'Command') {
        # Set the cursor to a blinking block.
        Write-Host -NoNewLine "$Esc[1 q"
    } else {
        # Set the cursor to a blinking line.
        Write-Host -NoNewLine "$Esc[5 q"
    }
}
Set-PSReadLineOption -ViModeIndicator Script -ViModeChangeHandler $Function:OnViModeChangeSetCursor


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
      # Representation of modifiers (like shift) when ReadKey uses IncludeKeyDown
      if ($key.Character -eq 0x00) {
        return
      } else {
        # Insert func above converts escape characters to their literals, e.g.
        # converts return to ^M. This doesn't.
        $wshell = New-Object -ComObject wscript.shell
        $wshell.SendKeys("{$($key.Character)}")
      }
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
Set-PSReadLineOption -Colors @{ Command = [ConsoleColor]::White }

