#Requires AutoHotkey v2.0
#Warn  ; Enable warnings to assist with detecting common errors.
KeyHistory(0)
#SingleInstance Force
; #NoTrayIcon
#InputLevel 0
#UseHook true
SendMode("Input")  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir(A_MyDocuments "\..")

#Include %A_Scriptdir%\lib.ahk

A_HotkeyModifierTimeout := 10
A_MenuMaskKey := "vkFF"

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

if !IsSet(nvimBinaryPath){
    nvimBinaryPath := FindProgramInPath("nvim.exe")
}

vim(args:=""){
    try {
        ; For better startup perf, specify nvim binary path.
        nvimBinaryArg := ""
        if nvimBinaryPath != "" {
            nvimBinaryArg := Format("--neovim-bin `"{1}`"", nvimBinaryPath)
        }
        Run("neovide.exe " . nvimBinaryArg . " -- " . args)
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
    Run(EnvGet("LOCALAPPDATA") . "\Programs\signal-desktop\Signal.exe --use-tray-icon")
}
chat := signal

keepass(){
    Run("C:\Program Files\KeePassXC\keepassxc.exe")
    WinWait("KeePassXC")
    Send("^f")
}
passwords := keepass
; Include if exists - intended to be local to each machine
; Example: Google suite, override and add new
; GCalendar(){
;     Run C:\Users\user\Desktop\Google Calendar.lnk
; }
; calendar=GCalendar
; GChat(){
;     Run C:\Users\user\Desktop\Google Chat.lnk
; }
; chat=GChat
; #If IsPrefix("exe")
; m::Run C:\Users\user\Desktop\Gmail.lnk
; c::Run C:\Users\user\Desktop\Google Meet.lnk
; #If

WorkraveToggle(){

    if (ProcessExist("workrave.exe")){
        ProcessClose("workrave.exe")
    }
    else
    {
        Run "workrave.exe"
    }
}

#Include %A_Scriptdir%\programs_hotkeys.ahk

#Include *i %A_MyDocuments%\local shortcuts.ahk

; If colemak.ahk is in use, reload it to make sure its keys trigger these ones properly.
script_reload(A_scriptdir "\colemak.ahk")
