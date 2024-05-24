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
