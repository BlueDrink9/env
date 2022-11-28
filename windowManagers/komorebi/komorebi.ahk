#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#KeyHistory 0
#SingleInstance Force
#NoTrayIcon

configDir := A_MyDocuments "\..\.config\komorebi"
; convenience function library
#include %A_MyDocuments%\..\.config\komorebi\komorebic.lib.ahk
; Crowd-sourced app configs
#include %A_MyDocuments%\..\.config\komorebi\komorebi.generated.ahk
; #include %configDir%\komorebi.generated.ahk

resetBorderColour(){
  ; this is a nice blue colour #42A5F5
  activeWindowBorderColour(66, 165, 245, "single")
}
resetBorderColour()

run komorebic.exe complete-configuration

#include %A_ScriptDir%\bindings.ahk
