Komo(cmd, params*) {
    cmdArgs := ""
    for index,param in params {
        cmdArgs .= param . " "
    }
    RunWait(format("komorebic.exe {}", cmd . " " . cmdArgs), , "Hide")
}

directionMap := Map("l", "right", "h", "left", "k", "up", "j", "down")
hotkeyPrefixes := Map("", "focus", "+", "move", "^", "send")
cycleTargetHotkeyPrefix := Map("", "workspace", "!", "monitor")
cycleDirectionMap := Map("[", "previous", "]", "next", "SC01A", "previous")

SplitModAndDirkey(ThisHotkey, &modifiers, &dirKey){
  ; Removes the # from the start of the key, then sets modifiers to the
  ; remaining modifiers keys (e.g. '+!'), and dirKey to the final keypress.
  ; StrReplace() is not case sensitive
  ; check for StringCaseSense in v1 source script
  ; and change the CaseSense param in StrReplace() if necessary
  thisHotkey := StrReplace(thisHotkey, "#")
  dirKey := SubStr(thisHotkey, -1, 1) ; last character
  modifiers := SubStr(thisHotkey, 1, -1) ; all but last char
}

; focus,swap, or move/warp the node in the given direction.
; Expects keys like #h or #+h. If it is shifted (+), will move.
; Otherwise, focuses.
FocusOrMoveDirection(ThisHotkey){
  SplitModAndDirkey(A_ThisHotkey, &modifiers, &dirKey)
  command := hotkeyPrefixes[modifiers]
  Komo(command " " directionMap[dirKey])
}

for _, key in ["h", "l", "k", "j"] {
  ; focus
  Hotkey "#" . key, FocusOrMoveDirection
  ; move
  Hotkey "#+" . key, FocusOrMoveDirection
}

CycleDirection(){
  ; StrReplace() is not case sensitive
  ; check for StringCaseSense in v1 source script
  ; and change the CaseSense param in StrReplace() if necessary
  thisHotkey := StrReplace(A_ThisHotkey, "#")
  SplitModAndDirkey(A_ThisHotkey, &modifiers, &dirKey)
  command := "cycle-" cycleTargetHotkeyPrefix[modifiers]
  Komo(command " " cycleDirectionMap[dirKey])
}

FocusOrMoveWorkspaceNumber(){
  ; StrReplace() is not case sensitive
  ; check for StringCaseSense in v1 source script
  ; and change the CaseSense param in StrReplace() if necessary
  spaceNum := StrReplace(A_ThisHotkey, "#")
  SplitModAndDirkey(A_ThisHotkey, &modifiers, &spaceNum)
  FocusAndMove := false
  command := hotkeyPrefixes[modifiers] "-workspace"
  Komo(command " " spaceNum)
}

; loop, 9{
;   ; focus
;   Hotkey, #%A_index%, FocusOrMoveWorkspaceNumber
;   ; move
;   Hotkey, #+%A_index%, FocusOrMoveWorkspaceNumber
;   ; focus and move
;   Hotkey, #+^%A_index%, FocusOrMoveWorkspaceNumber
; }

; for _, key in ["[", "]"] {
;   ; desktop/workspace
;   Hotkey, #%key%, CycleDirection
;   ; monitor
;   Hotkey, #!%key%, CycleDirection
;   ; Hotkey, #+%key%, FocusOrMoveWorkspaceDirection
;   ; Hotkey, #+!%key%, FocusOrMoveWorkspaceDirection
; }

; Hotkey, #+t, CycleDirection
#[::Komo("cycle-workspace", "previous")
#]::Komo("cycle-workspace", "next")
#+[::Komo("cycle-send-to-workspace", "previous")
#+]::Komo("cycle-send-to-workspace", "next")
#^[::Komo("cycle-move-to-workspace", "previous")
#^]::Komo("cycle-move-to-workspace", "next")
#![::Komo("cycle-monitor", "previous")
#!]::Komo("cycle-monitor", "next")
#!h::Komo("cycle-monitor", "previous")
#!l::Komo("cycle-monitor", "next")
#!+[::Komo("cycle-send-to-monitor", "previous")
#!+]::Komo("cycle-send-to-monitor", "next")
#!^[::Komo("cycle-move-to-monitor", "previous")
#!^]::Komo("cycle-move-to-monitor", "next")

; Probably actually generally useful
#z::Komo("minimize", )
#x::Komo("close", )
#!x::Send("#x")

#!Esc::Komo("stop", )
; if things get buggy, often a retile will fix it
#^c::Komo("retile", )
; Hard restart
#^!c::{
  Run("taskkill /f /im komorebi.exe")
  Run("taskkill /f /im komorebi.exe")
  Run("taskkill /f /im komorebi.exe")
  Run("powershell -NoProfile -windowStyle hidden -File `"" A_ScriptDir "\komorebi.ps1`"")
  return
}
; Reloads ~/komorebi.ahk (I'm hoping it's more flexible than that...)
#+c::{
  Komo("reload-configuration", )
  Run("powershell -NoProfile -windowStyle hidden -File `"" A_ScriptDir "\settings.ps1`"")
  Reload()
  Komo("restore-windows", )
  return
}

; Force resourcing app-specific configs, since they seem so very flakey
#c::Run("powershell -NoProfile -windowStyle hidden -File `"" A_AppData "\..\..\.config\komorebi\komorebi.generated.ps1`"")
; Force Grab/manage window
#!m::Komo("manage", )
#!i::Komo("un-manage", )


; #w::
;   ; Yellowy Orange #F88F08
;   activeWindowBorderColour(248, 143, 8, "single")
;   ; Sequence({w: "komorebic.exe quick-save"
;   ; ; loads $Env:TEMP\komorebi.quicksave.json on the focused workspace
;   ; , e: "komorebic.exe quick-load"})
;   ; resetBorderColour()
; return

#+m::Komo("toggle-monocle", )
#+!m::Komo("toggle-maximize", )
#+t::Komo("toggle-tiling", )
#+f::Komo("toggle-float", )

#n::Komo("new-workspace", )

#+r::Komo("flip-layout", "horizontal-and-vertical")
; #r::Komo("flip-layout", "horizontal")
; #r::Komo("flip-layout", "vertical")

; Block windows+l from locking, with colemak.ahk
; Add this in %A_MyDocuments%\local colemak overrides.ahk to get win+l working with colemak.ahk
; #if true
; #u::RunWait, komorebic.exe focus right, , Hide
; #if
#l::Komo("focus", "right")
