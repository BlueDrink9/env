#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
; SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#KeyHistory 0
#SingleInstance Force
; #NoTrayIcon

; $HOME, hopefully/usually...
SetWorkingDir %A_MyDocuments%\..

#inputlevel 0

; resetBorderColour(){
;   ; this is a nice blue colour #42A5F5
;   activeWindowBorderColour(66, 165, 245, "single")
; }
; resetBorderColour()

; Include autogenerated libraries
; convenience function library
#include %A_AppData%\..\..\.config\komorebi\komorebic.lib.ahk


#include *i %A_MyDocuments%\..\.config\komorebi\komorebi_before.ahk
#include %A_scriptdir%\..\..\AutoHotkey\programs_setup.ahk
#include %A_scriptdir%\bindings_autoexec.ahk

; END autoexec section

; we want komorebi to override prefix keys where relevant, I think.
#include *i %A_MyDocuments%\local shortcuts.ahk
#include %A_scriptdir%\..\..\AutoHotkey\programs_hotkeys.ahk

#include %A_ScriptDir%\bindings.ahk
#include *i %A_MyDocuments%\..\.config\komorebi\komorebi_after.ahk
