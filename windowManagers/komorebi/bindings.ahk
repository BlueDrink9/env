; Hotkey, #+t, CycleDirection
#[::CycleWorkspace("previous")
#]::CycleWorkspace("next")
#+[::CycleSendToWorkspace("previous")
#+]::CycleSendToWorkspace("next")
#^[::CycleMoveToWorkspace("previous")
#^]::CycleMoveToWorkspace("next")
#![::CycleMonitor("previous")
#!]::CycleMonitor("next")
#!h::CycleMonitor("previous")
#!l::CycleMonitor("next")
#!+[::CycleSendToMonitor("previous")
#!+]::CycleSendToMonitor("next")
#!^[::CycleMoveToMonitor("previous")
#!^]::CycleMoveToMonitor("next")

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
  Reload
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
