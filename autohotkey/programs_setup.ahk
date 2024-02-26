prefix := ""


ResetPrefix(){
    global prefix
    prefix := ""
    ToolTip()
}

SetPrefix(arg){
    global prefix
    ToolTip(arg)
    prefix := arg
    SetTimer(ResetPrefix,-1000)
}

IsPrefix(arg){
    global prefix
    ; TODO have a list of acceptable prefixes, verify prefix is in that list.
    ; Will eliminate risk of error due to typo.
    return prefix = arg
}

GetDefaultBrowser() {
    BrowserPath := RegRead("HKCU\Software\Microsoft\Windows\Shell\Associations\UrlAssociations\http\UserChoice", "ProgId")
    maps := map("IE.HTTP", "iexplore.exe" , "FirefoxURL", "firefox.exe" , "ChromeHTML", "chrome.exe" , "AppXq0fevzme2pys62n3e0fbqa7peapykr8v", "microsoft-edge.exe" , "BraveHTML", "brave.exe" )
    for ProgId, browser in maps {
        if RegExMatch(BrowserPath, ".*" . ProgId . ".*"){
            BrowserPath := browser
        }
    }
    return BrowserPath
}

; Default exec functions.
; Variables can be overridden by local functions

vim(args:=""){
    try {
        ; Don't really need to fork, and it has a slight performance hit.
        ; For better startup perf, specify nvim binary path.
        nvimBinaryArg := ""
        if nvimBinaryPath {
            nvimBinaryArg := Format("--neovim-bin `"{1}`"", nvimBinaryPath)
        }
        Run("neovide.exe " . nvimBinaryArg . " --nofork -- " args)
    } catch {
        Run("gvim.exe", args)
    }
}

liteEditor(){
    vim("--cmd `"let g:liteMode=1`"")
}
editor(){
    vim()
}
ide(){
    vim("--cmd `"let g:ideMode=1`"")
}

alacritty(admin:=false){
    ; run, "WindowsTerminal.lnk"
    if admin{
        Run("*RunAs `"alacritty.exe`"")
    } else {
        Run("alacritty.exe")
    }
}
terminal := alacritty

joplin(){
    try {
        Run("joplin.exe")
    } catch {
        Run(A_Programs . "\Joplin.lnk")
    }
}
notes := joplin

calendar := "thunderbird"
thunderbird(){
    Run("Thunderbird")
}

signal(){
    global
    Run(A_appdata . "\Programs\signal-desktop\Signal.exe --use-tray-icon")
}
chat := signal

keepass(){
    Run("C:\Program Files\KeePassXC\keepassxc.exe")
    WinWait("KeePassXC")
    Send("^f")
}
passwords := keepass
