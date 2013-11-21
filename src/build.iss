; This file is only included in release builds

#sub Sign
  #if Exec(SignCmd + " " + Module, "", SourcePath) != 0
    #pragma error "Code signing failed: " + AddBackslash(SourcePath) + Module
  #endif
  #pragma message "Signed: " + AddBackslash(SourcePath) + Module
#endsub

; release defines
#define OutputDir "release"
#define OutputBaseFilename AppInstallName + "." + SetupVersion

; release guard
#if FileExists(OutputDir + "\" + OutputBaseFilename + ".exe")
  #error This version has already been released
#endif

; code sign dlls
#define protected Module "userdata.dll"
#expr Sign

#define protected Module "shellext\Win32\Release\" + ShellExt32
#expr Sign

#define protected Module "shellext\x64\Release\" + ShellExt64
#expr Sign
