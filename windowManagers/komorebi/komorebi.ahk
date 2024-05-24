#Requires AutoHotkey v2.0.2
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode("Input")  ; Recommended for new scripts due to its superior speed and reliability.
; SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
KeyHistory(0)
#SingleInstance Force
; #NoTrayIcon

; $HOME, hopefully/usually...
SetWorkingDir(A_MyDocuments "\..")

#InputLevel 0

; resetBorderColour(){
;   ; this is a nice blue colour #42A5F5
;   activeWindowBorderColour(66, 165, 245, "single")
; }
; resetBorderColour()

#Include *i %A_MyDocuments%\..\.config\komorebi\komorebi_before.ahk
; Include my standard ahk programs setup script, because if I'm running komo I also will want this.
#Include %A_scriptdir%\..\..\AutoHotkey\programs_setup.ahk
#Include %A_scriptdir%\..\..\AutoHotkey\programs_hotkeys.ahk

; we want komorebi to override prefix keys where relevant, I think. So load komo bindings after program setup
#Include *i %A_MyDocuments%\local shortcuts.ahk
#Include %A_ScriptDir%\bindings.ahk


; Function to find and reload colemak.ahk if it's running, so that its
; bindings send komorebi ones.
; Get the list of running AHK scripts
for process in ComObjGet("winmgmts:").ExecQuery("Select * from Win32_Process where Name = 'AutoHotkey64.exe'") {
    ; Sometimes the Command Line might be unavailable (null), handle this case
    if (process.CommandLine) {
        if (InStr(process.CommandLine, "colemak.ahk")) {
            scriptPid := process.ProcessID
            target := "ahk_pid " . scriptPid
            DetectHiddenWindows(true)
            if WinExist(target) {
              ProcessClose(scriptPid)
                Sleep(1000) ; Wait for a second to let the process close gracefully
                Run(A_AhkPath . " " . A_scriptdir . "\..\..\AutoHotkey\colemak.ahk")
            }
          return
        }
    }
}


#Include *i %A_MyDocuments%\..\.config\komorebi\komorebi_after.ahk
