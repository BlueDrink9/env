#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#KeyHistory 0
#SingleInstance Force
; #NoTrayIcon Since if you're running this script directly, you're debugging

; convenience function library
#include %A_MyDocuments%\..\.config\komorebi\komorebic.lib.ahk

#include %A_scriptdir%\..\..\autohotkey\hotkey_sequence.ahk

directionMap := {l: "right", h: "left", k: "up", j: "down"}
hotkeyPrefixes := {"": "Focus"
  ,"+": "Move"
  ,"^": "Send"}
cycleTargetHotkeyPrefix := {"": "Workspace", "!": "Monitor"}
cycleDirectionMap := {"[": "previous", "]": "next"}

SplitModAndDirkey(ThisHotkey, byref modifiers, byref dirKey){
  ; Removes the # from the start of the key, then sets modifiers to the
  ; remaining modifiers keys (e.g. '+!'), and dirKey to the final keypress.
  StringReplace, thisHotkey, thisHotkey, #,, All
  dirKey := SubStr(thisHotkey, 0, 1) ; last character
  modifiers := SubStr(thisHotkey,1,-1) ; all but last char
}

; focus,swap, or move/warp the node in the given direction.
; Expects keys like #h or #+h. If it is shifted (+), will move.
; Otherwise, focuses.
FocusOrMoveDirection(){
  global directionMap, hotkeyPrefixes
  SplitModAndDirkey(A_ThisHotkey, modifiers, dirKey)
  command := hotkeyPrefixes[modifiers]
  %command%(directionMap[dirKey])
}

CycleDirection(){
  global cycleDirectionMap, cycleTargetHotkeyPrefix
  StringReplace, thisHotkey, A_ThisHotkey, #,, All
  SplitModAndDirkey(A_ThisHotkey, modifiers, dirKey)
  command := "Cycle" + cycleTargetHotkeyPrefix[modifiers]
  %command%(cycleDirectionMap[dirKey])
}

FocusOrMoveWorkspaceNumber(){
  global hotkeyPrefixes
  StringReplace, spaceNum, A_ThisHotkey, #,, All
  SplitModAndDirkey(A_ThisHotkey, modifiers, spaceNum)
  FocusAndMove := false
  command := hotkeyPrefixes[modifiers] + "Workspace"
  %command%(spaceNum)
}

for _, key in ["h", "l", "k", "j"] {
  ; focus
  Hotkey, #%key%, FocusOrMoveDirection
  ; move
  Hotkey, #+%key%, FocusOrMoveDirection
}

for _, key in ["[", "]"] {
  ; desktop/workspace
  Hotkey, #%key%, CycleDirection
  ; monitor
  Hotkey, #!%key%, CycleDirection
  ; Hotkey, #+%key%, FocusOrMoveWorkspaceDirection
  ; Hotkey, #+!%key%, FocusOrMoveWorkspaceDirection
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
