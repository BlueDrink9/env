#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#KeyHistory 0
#SingleInstance Force
; #NoTrayIcon

prefix := ""

SetPrefix(arg){
    global prefix
    prefix := arg
    SetTimer, ResetPrefix, 300
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

#If (IsPrefix(""))
#e::SetPrefix("exe")
#o::SetPrefix("office")
#If

ResetPrefix:
    prefix := ""
return

#If IsPrefix("office")
w::Run, winword.exe
p::Run, powerpnt.exe
s::Run, excel.exe
#If

#If IsPrefix("exe")
e::Run explorer
t::Run wt.exe
b::Run % GetDefaultBrowser()
v::Run % gvim.exe --cmd "let g:liteMode=1"
+v::Run % gvim.exe
i::Run % gvim.exe --cmd "let g:IDEMode=1"
#If

