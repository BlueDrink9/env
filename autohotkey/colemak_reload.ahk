; Function to find and reload colemak.ahk if it's running, so that its
; bindings trigger other AHK scripts as expected.
; Get the list of running AHK scripts
for process in ComObjGet("winmgmts:").ExecQuery("Select * from Win32_Process where Name = 'AutoHotkey64.exe'") {
    ; Sometimes the Command Line might be unavailable (null), handle this case
    if (process.CommandLine) {
        if (InStr(process.CommandLine, "colemak.ahk")) {
            scriptPid := process.ProcessID
            target := "ahk_pid " . scriptPid
            DetectHiddenWindows(true)
            if WinExist(target) {
              ProcessClose(scriptPid)
                Sleep(1000) ; Wait for a second to let the process close gracefully
                Run(A_AhkPath . " " . A_scriptdir . "\colemak.ahk")
            }
          return
        }
    }
}

