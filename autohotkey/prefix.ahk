#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_MyDocuments%
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
t::Run wt.exe
b::Run % GetDefaultBrowser()
v::Run gvim.exe, --cmd "let g:liteMode=1"
+v::Run gvim.exe
n::Run joplin.exe
i::
    try{
        Run neovide.exe -- --cmd "let g:IDEMode=1"
    } catch {
        Run gvim.exe, --cmd "let g:IDEMode=1"
    }
return
#If

#If IsPrefix("media")
s::Run spotify.exe
#If

