# RunPhp

The program `runphp.exe` is used to check that a selected php.exe can actually be run. If certain
components are missing (for example the required Visual C++ Redistributable dlls), php will not
start and the system will show an error dialog, which in silent installations would cause the
process to hang indefinitely. Likewise, incompatible extension dlls can also result in dialog boxes.

`runphp.exe` guards against this by using a 30-second time-out and setting error mode flags to try
and suppress any dialog boxes. It is needed because Inno Setup does not currently provide this
functionality and actually makes things worse by calling CreateProcess with the `dwCreationFlags`
set to CREATE_DEFAULT_ERROR_MODE.

The program requires `path\to\php.exe` as it first argument plus the commands to run. It returns the
exit code from PHP, the value from GetLastError if the process cannot be created, or NTSTATUS
values for other errors (specifically `STATUS_IO_TIMEOUT` if the wait times out and
`STATUS_UNSUCCESSFUL` if GetExitCodeProcess fails).
