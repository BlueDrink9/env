#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#KeyHistory 0
#SingleInstance Force
#NoTrayIcon

#include %A_scriptdir%\include_libs.ahk

#inputlevel 1

resetBorderColour(){
  ; this is a nice blue colour #42A5F5
  activeWindowBorderColour(66, 165, 245, "single")
}
resetBorderColour()

run komorebic.exe complete-configuration

; #include %A_MyDocuments%\..\.config\komorebi\komorebi_before.ahk
#include %A_ScriptDir%\bindings.ahk
#include %A_MyDocuments%\..\.config\komorebi\komorebi_after.ahk
