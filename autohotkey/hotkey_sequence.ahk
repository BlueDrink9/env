#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#KeyHistory 0
#SingleInstance Force
#NoTrayIcon

/* Accepts an associative array of a sequence of letters to press, followed by a
* command to run once each of the letters in the sequenec has been pressed.
* Written by the great Lexikos,
* https://www.autohotkey.com/boards/viewtopic.php?p=295127#p295127
* Example usage:
* !r::Sequence({e: "explorer.exe"
*    , bc: "chrome.exe"
*    , bf: "firefox.exe"})
*/
Sequence(cmdmap) {
    matchlist := ""
    max_sequence_len := 1
    for keys, cmd in cmdmap {
        matchlist .= "," keys
        if (max_sequence_len < StrLen(keys))
            max_sequence_len := StrLen(keys)
    }
    Input command, % "L" max_sequence_len,, % SubStr(matchlist, 2)
    if command := cmdmap[command]
        Run % command
}

