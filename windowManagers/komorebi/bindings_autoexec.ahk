directionMap := {l: "right", h: "left", k: "up", j: "down"}
hotkeyPrefixes := {"": "Focus"
  ,"+": "Move"
  ,"^": "Send"}
cycleTargetHotkeyPrefix := {"": "Workspace", "!": "Monitor"}
cycleDirectionMap := {"[": "previous", "]": "next", "SC01A": "previous"}

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
  msgbox, 'hi'
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

