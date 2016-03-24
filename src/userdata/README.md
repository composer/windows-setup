# Userdata

The `userdata.dll` handles the deletion of multi-user data folders, displaying a progress indicator and a list of any failed operations. It is written in Pascal and must be compiled with a 32-bit Unicode Delphi.

The dll can be debugged by setting `HostDebug.exe` as the host application. This app mimics the uninstaller behaviour by searching for Composer user data (only for the current user) and passing the folder list to the dll.

The dll is used because of the difficulty in coding something similar using just Inno functionality that gives the same user-experience.
