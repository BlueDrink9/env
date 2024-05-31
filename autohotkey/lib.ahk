; Function to search for the program in directories listed in the PATH
FindProgramInPath(programName) {
    ; Read the PATH environment variable from the registry
    pathVar := RegRead("HKEY_LOCAL_MACHINE", "System\CurrentControlSet\Control\Session Manager\Environment", "PATH")
    ; Split the PATH variable into an array of directories
    paths := StrSplit(pathVar, ";")
    for each, path in paths {
        ; Check if the program exists in the current directory
        filePath := path . "\" . programName
        if FileExist(filePath) {
            return filePath
        }
    }
    ; Return empty string if program is not found in any directory
    return ""
}


script_reload(script_path){
    ; Function to find and reload colemak.ahk if it's running, so that its
    ; bindings trigger other AHK scripts as expected.
    SplitPath script_path, &script_name
    ; Get the list of running AHK scripts
    for process in ComObjGet("winmgmts:").ExecQuery("Select * from Win32_Process where Name = 'AutoHotkey64.exe'") {
        ; Sometimes the Command Line might be unavailable (null), handle this case
            if (process.CommandLine) {
                if (InStr(process.CommandLine, script_name)) {
scriptPid := process.ProcessID
                target := "ahk_pid " . scriptPid
                DetectHiddenWindows(true)
                if WinExist(target) {
                    ProcessClose(scriptPid)
                    Sleep(1000) ; Wait for a second to let the process close gracefully
                    Run(A_AhkPath . " " . script_path)
                }
            return
            }
        }
    }
}
