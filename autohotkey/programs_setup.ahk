prefix := ""

SetPrefix(arg){
    global prefix
    tooltip % arg
    prefix := arg
    SetTimer, ResetPrefix, -1000
}

IsPrefix(arg){
    global prefix
    ; TODO have a list of acceptable prefixes, verify prefix is in that list.
    ; Will eliminate risk of error due to typo.
    return prefix = arg
}

GetDefaultBrowser() {
    RegRead, BrowserPath, HKCU, Software\Microsoft\Windows\Shell\Associations\UrlAssociations\http\UserChoice, ProgId
    maps := { "IE.HTTP": "iexplore.exe"
        , "FirefoxURL": "firefox.exe"
        , "ChromeHTML": "chrome.exe"
        , "AppXq0fevzme2pys62n3e0fbqa7peapykr8v": "microsoft-edge.exe"
        , "BraveHTML": "brave.exe" }
    for ProgId, browser in maps {
        if regexmatch(BrowserPath, ".*" . ProgId . ".*"){
            BrowserPath := browser
        }
    }
    return BrowserPath
}

; Default exec functions.
; Variables can be overridden by local functions

vim(args:=""){
    try {
        Run neovide.exe -- %args%
    } catch {
        Run gvim.exe, %args%
    }
}

liteEditor=liteEditor
liteEditor(){
    vim("--cmd ""let g:liteMode=1""")
}
editor=editor
editor(){
    vim()
}
ide=ide
ide(){
    vim("--cmd ""let g:ideMode=1""")
}

terminal=terminal
terminal(admin:=false){
    ; run, "WindowsTerminal.lnk"
    if admin{
        run, *RunAs "alacritty.exe"
    } else {
        run, "alacritty.exe"
    }
}

notes=joplin
joplin(){
    Run joplin.exe
}

calendar=calendar
calendar(){
    Run "Thunderbird"
}

chat=chat
chat(){
    global
    Run "%L_appdata%\Programs\signal-desktop\Signal.exe" --use-tray-icon
}
