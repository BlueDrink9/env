# Self-elevate the script if required
if (-Not ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] 'Administrator')) {
 if ([int](Get-CimInstance -Class Win32_OperatingSystem | Select-Object -ExpandProperty BuildNumber) -ge 6000) {
  $CommandLine = "-File `"" + $MyInvocation.MyCommand.Path + "`" " + $MyInvocation.UnboundArguments
  Start-Process -FilePath PowerShell.exe -Verb Runas -ArgumentList $CommandLine
  Exit
 }
}
$VSCODE_APP_DATA="${HOME}/AppData/Roaming/vscodium/User"
New-Item -ItemType SymbolicLink -Path "$VSCODE_APP_DATA/settings.json" -Target "$PSScriptRoot/settings.json"
New-Item -ItemType SymbolicLink -Path "$VSCODE_APP_DATA/keybindings.json" -Target "$PSScriptRoot/keybindings.json"
New-Item -ItemType SymbolicLink -Path "$VSCODE_APP_DATA/snippets" -Target "$PSScriptRoot/snippets"
