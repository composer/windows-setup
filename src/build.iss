; This file is only included in release builds

#sub Sign
  #if Exec(SignCmd + " " + Module, "", SourcePath) != 0
    #pragma error "Code signing failed: " + AddBackslash(SourcePath) + Module
  #endif
  #pragma message "Signed: " + AddBackslash(SourcePath) + Module
#endsub

; release defines
#define private OutputDir "release"
#define private OutputBaseFilename "Composer-Setup." + SetupVersion

; release guard
#if FileExists(OutputDir + "\" + OutputBaseFilename + ".exe")
  #pragma error "Version " + SetupVersion + " has already been released"
#endif

; code sign dlls
#define protected Module "userdata.dll"
#expr Sign

#define protected Module SettingsExe
#expr Sign

#define protected Module ShellExt32
#expr Sign

#define protected Module ShellExt64
#expr Sign


[Setup]
OutputDir={#OutputDir}
OutputBaseFilename={#OutputBaseFilename}
SignTool={#SignTool}
