#Warn  ; Enable warnings to assist with detecting common errors.
#KeyHistory 0
#SingleInstance Force
; #NoTrayIcon
#InputLevel 0
#UseHook on
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_MyDocuments%\..

#include %A_Scriptdir%\programs_setup.ahk

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
#include *i %A_MyDocuments%\local shortcuts.ahk

#include %A_Scriptdir%\programs_hotkeys.ahk


