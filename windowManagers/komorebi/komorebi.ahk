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

#Include %A_scriptdir%\..\..\AutoHotkey\colemak_reload.ahk

#Include *i %A_MyDocuments%\..\.config\komorebi\komorebi_after.ahk
