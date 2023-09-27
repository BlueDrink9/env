#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
; SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
; SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#KeyHistory 0
#SingleInstance Force
; #NoTrayIcon Since if you're running this script directly, you're debugging


; Hotkey, #+t, CycleDirection
#[::CycleWorkspace("previous")
#]::CycleWorkspace("next")
#+[::CycleMoveToWorkspace("previous")
#+]::CycleMoveToWorkspace("next")
#^[::
  CycleMoveToWorkspace("previous")
  CycleWorkspace("previous")
return
#^]::
  CycleMoveToWorkspace("next")
  CycleWorkspace("next")
return
#![::CycleMonitor("previous")
#!]::CycleMonitor("next")
#!+[::CycleMoveToMonitor("previous")
#!+]::CycleMoveToMonitor("next")
#!^[::
  CycleMoveToMonitor("previous")
  CycleMonitor("previous")
return
#!^]::
  CycleMoveToMonitor("next")
  CycleMonitor("next")
return

; Probably actually generally useful
#z::Minimize()
#x::close()
#!x::send #x

#!Esc::Stop()
; if things get buggy, often a retile will fix it
#^c::Retile()
; Reloads ~/komorebi.ahk (I'm hoping it's more flexible than that...)
#+c::
  ReloadConfiguration()
  run, powershell -NoProfile -windowStyle hidden -File "%A_ScriptDir%\settings.ps1"
return
; Force Grab/manage window
#!\::Manage()
#!+\::UnManage()


; #w::
;   ; Yellowy Orange #F88F08
;   activeWindowBorderColour(248, 143, 8, "single")
;   ; Sequence({w: "komorebic.exe quick-save"
;   ; ; loads $Env:TEMP\komorebi.quicksave.json on the focused workspace
;   ; , e: "komorebic.exe quick-load"})
;   ; resetBorderColour()
; return

#+m::toggleMonocle()
#+!m::toggleMaximize()
#+t::toggleTiling()
#+f::toggleFloat()

#n::NewWorkspace()

#+r::FlipLayout("horizontal-and-vertical")
; #r::FlipLayout("horizontal")
; #r::FlipLayout("vertical")

; Block windows+l from locking, with colemak.ahk
; Add this in %A_MyDocuments%\local colemak ove rrides.ahk to get win+l working with colemak.ahk
; #if true
; #u::RunWait, komorebic.exe focus right, , Hide
; #if
#l::Focus("right")
