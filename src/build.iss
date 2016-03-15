; This file is only included in release builds

#sub Sign
  ; sign SHA1
  #if Exec(SignExe + " " + SignSha1 + " " + Module, "", SourcePath) != 0
    #pragma error "Code signing failed: " + AddBackslash(SourcePath) + Module
  #endif
  #pragma message "Signed SHA1: " + AddBackslash(SourcePath) + Module

  ; sign SHA2
  #if Exec(SignExe + " " + SignSha2 + " " + Module, "", SourcePath) != 0
    #pragma error "Code signing failed: " + AddBackslash(SourcePath) + Module
  #endif
  #pragma message "Signed SHA2: " + AddBackslash(SourcePath) + Module
#endsub

; release defines
#define private OutputDir "release"
#define private OutputBaseFilename "Composer-Setup." + SetupVersion

; release guard
#if FileExists(OutputDir + "\" + OutputBaseFilename + ".exe")
  #pragma error "Version " + SetupVersion + " has already been released"
#endif

; code sign userdata.dll
#define protected Module "userdata.dll"
#expr Sign


[Setup]
OutputDir={#OutputDir}
OutputBaseFilename={#OutputBaseFilename}
SignTool={#SignTool} {#SignExe} {#SignSha1} $f
SignTool={#SignTool} {#SignExe} {#SignSha2} $f
