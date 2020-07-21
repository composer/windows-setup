@echo off

:: remove inheritance
icacls %ALLUSERSPROFILE%\ComposerSetup /inheritance:d /Q

:: remove creator owner
icacls %ALLUSERSPROFILE%\ComposerSetup /remove:g *S-1-3-0 /Q

:: remove builtin\users
icacls %ALLUSERSPROFILE%\ComposerSetup /remove:g *S-1-5-32-545 /Q

:: grant read-execute to builtin\users 
icacls %ALLUSERSPROFILE%\ComposerSetup /grant *S-1-5-32-545:(OI)(CI)(RX) /Q
