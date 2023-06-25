; #Warn  ; Enable warnings to assist with detecting common errors.
; #KeyHistory 0
; #SingleInstance Force
; #NoTrayIcon

SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_MyDocuments%

prefix := ""

SetPrefix(arg){
    global prefix
    prefix := arg
    SetTimer, ResetPrefix, 1000
}

IsPrefix(arg){
    global prefix
    return prefix = arg
}

GetDefaultBrowser() {
    RegRead, BrowserPath, HKCU, Software\Microsoft\Windows\Shell\Associations\UrlAssociations\http\UserChoice, ProgId
    if (BrowserPath = "IE.HTTP")
        BrowserPath := "iexplore.exe"
    else if (BrowserPath = "FirefoxURL")
        BrowserPath := "firefox.exe"
    else if (BrowserPath = "ChromeHTML")
        BrowserPath := "chrome.exe"
    else if (BrowserPath = "AppXq0fevzme2pys62n3e0fbqa7peapykr8v")
        BrowserPath := "microsoft-edge.exe"
    else if (BrowserPath = "BraveHTML")
        BrowserPath := "brave.exe"
    return BrowserPath
}

terminal := "WindowsTerminal.lnk"
terminal := "alacritty"

; Include if exists - intended to be local to each machine
#include *i %A_MyDocuments%\local shortcuts.ahk

; Example: Override vim command:
; #If IsPrefix("exe")
; hotkey, if, IsPrefix("exe")
; hotkey, v, editor
; editor:
;     Run neovide.exe -- --cmd "let g:liteMode=1"
; return
; #If

ResetPrefix:
    prefix := ""
return


#If (IsPrefix(""))
#e::SetPrefix("exe")
#o::SetPrefix("office")
#m::SetPrefix("media")
#If

#If IsPrefix("office")
w::Run, winword.exe
p::Run, powerpnt.exe
s::Run, excel.exe
#If

#If IsPrefix("exe")
e::Run explorer
t::Run "%terminal%"
b::Run % GetDefaultBrowser()
v::Run gvim.exe, --cmd "let g:liteMode=1"
+v::Run gvim.exe
i::
    try{
        Run neovide.exe -- --cmd "let g:IDEMode=1"
    } catch {
        Run gvim.exe, --cmd "let g:IDEMode=1"
    }
return
; notes
n::Run joplin.exe
; Games
g::Run "%L_AppData%\Playnite\Playnite.DesktopApp.exe"
; Kalendar
k::Run "Thunderbird"
; Signal
s::Run "%L_appdata%\Programs\signal-desktop\Signal.exe" --use-tray-icon
; Passwords
p::
    Run "C:\Program Files\KeePassXC\keepassxc.exe"
    winwait,KeePassXC
    send ^f
return
#If

#If IsPrefix("media")
s::Run spotify.exe
#If

