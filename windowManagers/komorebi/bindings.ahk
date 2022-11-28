#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#KeyHistory 0
#SingleInstance Force
; #NoTrayIcon Since if you're running this script directly, you're debugging

; convenience function library
#include %A_MyDocuments%\..\.config\komorebi\komorebic.lib.ahk

#include %A_scriptdir%\..\..\autohotkey\hotkey_sequence.ahk

directionMap := {l: "right", h: "left", k: "up", j: "down"}
WSDirectionMap := {"[": "previous", "]": "next"}
hotkeyPrefixes := {"": "Focus"
  ,"+": "Move"
  ,"^": "Send"}
cycleTargetHotkeyPrefix := {"": "Workspace", "!": "Monitor"}
cycleDirectionMap := {"[": "previous", "]": "next"}

; focus,swap, or move/warp the node in the given direction.
; Expects keys like #h or #+h. If it is shifted (+), will move.
; Otherwise, focuses.
FocusOrMoveDirection(){
  StringReplace, thisHotkey, A_ThisHotkey, #,, All
  dirKey := SubStr(thisHotkey, 0, 1)
  modifiers := SubStr(thisHotkey, -1)
  command := hotkeyPrefixes[modifiers]
  %command%(directionMap[dirKey])
}

CycleDirection(){
  StringReplace, thisHotkey, A_ThisHotkey, #,, All
  dirKey := SubStr(thisHotkey, 0, 1)
  modifiers := SubStr(thisHotkey, -1)
  command := "Cycle" + cycleTargetHotkeyPrefix[modifiers]
  %command%(cycleDirectionMap[dirKey])
}

FocusOrMoveWorkspaceNumber(){
  StringReplace, spaceNum, A_ThisHotkey, #,, All
  spaceNum := SubStr(thisHotkey, 0, 1)
  modifiers := SubStr(thisHotkey, -1)
  FocusAndMove := false
  command := hotkeyPrefixes[modifiers] + "Workspace"
  %command%(spaceNum)
}

for _, dirKey in ["h", "l", "k", "j"] {
  ; focus
  Hotkey, #%dirKey%, FocusOrMoveDirection
  ; move
  Hotkey, #+%dirKey%, FocusOrMoveDirection
}

for _, dirKey in ["[", "]"] {
  ; desktop/workspace
  Hotkey, #%dirKey%, CycleDirection
  ; monitor
  Hotkey, #!%dirKey%, CycleDirection
  ; Hotkey, #+%dirKey%, FocusOrMoveWorkspaceDirection
  ; Hotkey, #+!%dirKey%, FocusOrMoveWorkspaceDirection
}

loop, 9{
  ; focus
  Hotkey, #%A_index%, FocusOrMoveWorkspaceNumber
  ; move
  Hotkey, #+%A_index%, FocusOrMoveWorkspaceNumber
  ; focus and move
  Hotkey, #+^%A_index%, FocusOrMoveWorkspaceNumber
}

; Probably actually generally useful
#x::send !{F4}
#!x::send #x

#!Esc::Stop()
; if things get buggy, often a retile will fix it
#+^c::Retile()
; Reloads ~/komorebi.ahk (I'm hoping it's more flexible than that...)
#+c::ReloadConfiguration()


#w::
  ; Yellowy Orange #F88F08
  activeWindowBorderColour(248, 143, 8, "single")
  Sequence({w: "komorebic.exe quick-save"
  ; loads $Env:TEMP\komorebi.quicksave.json on the focused workspace
  , e: "komorebic.exe quick-load"})
  resetBorderColour()
return

#+m::toggleMonocle()
#+!m::toggleMaximize()
#+t::toggleTiling()
#+f::toggleFloat()

#n::NewWorkspace()

#r::FlipLayout("horizontal-and-vertical")
; #r::FlipLayout("horizontal")
; #r::FlipLayout("vertical")
