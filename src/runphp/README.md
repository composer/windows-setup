# RunPhp

The program `runphp.exe` is used to check that a selected php.exe can actually be run. If certain components are missing (for example the required Visual C++ Redistributable dlls), php will not start and the system will show an error dialog, which in silent installations would cause the process to hang indefinitely.

`runphp.exe` guards against this by setting a time-out in silent installations and using error mode flags to suppress dialog boxes. It is needed because Inno Setup does not currently provide this functionality.
