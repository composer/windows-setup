## v3.1
2014-01-24

* Added a Settings item to the Shell Menus, under Composer Options. This displays `settings.exe` which handles the new features described below. 
* Added Default Console property to Shell Menus, allowing the use of console programs other than Command Prompt. Cygwin, Git Bash, Msys and PowerShell are supported out of the box (if installed) with a mechanism to use other programs like Console2.
* Added Collapse property to Shell Menus, to display an initial submenu and save space on the main context-menu.
* Added settings.exe dialog to implement new Shell Menu properties and provide setup Change functionality.
* Fixed bug that prevented uninstall on XP - [Issue #30](https://github.com/johnstevenson/composer-setup/issues/30)
* Previous-data ShellExt removed. Superseded by looking in registry for Shell Menus CLSID.


## v3.0
2013-11-22

* Major version pushed to v3, reflecting changes below and the requirement to uninstall any existing installation.
* Added optional Shell Menus context-menu handler, to allow Composer usage from Windows Explorer/File Manager. Not available on pre-Vista.
* Added dialog requiring user to shutdown programs that are locking context-menu dll.
* Added Change feature to installer, to allow Composer reinstall and to add/remove context-menus.
* Changed installation directories to split files into application (shell, installer, uninstaller etc) and bin (composer.phar, shims) categories. For admin installs these are `<ProgramFiles>\ComposerSetup` and `%ProgramData%\ComposerSetup\bin`. For user installs these are `%LOCALAPPDATA%\ComposerSetup` and `%LOCALAPPDATA%\ComposerSetup\bin`. The base folder name has been changed to ComposerSetup to avoid any conflicts in the Program Files directory. 
* Previous-data added: shell menus installed (key: ShellExt).
* Previous-data added: application directory (key: AppDir).
* Previous-data added: bin directory (key: BinDir).
* Code signing added to all dlls for release versions.
* Thanks to [cmenning](https://github.com/cmenning) for suggesting the context-menu handler.

## v2.8
2013-10-19

* Added support for cygwin php.
* Removed Start menu entry (automatically upgraded when re-installing over an older version)
* Moved released versions back to Github now they have introduced a Releases feature.
* Minor code tweaks.

## v2.7
2012-02-20

* Added an option to delete user data on uninstall. Uses a separate dll (userdata.dll) to get round Inno limitations which provides a cancellable progress form and error report. Only user data found at default locations is deleted. Config entries that point elsewhere are displayed for information but not deleted, and neither is the config.json file. Config entries at project level are not reported. For Admin uninstalls user data is shown for all users, where it can be reliably found.
* Moved User installation directory from roaming folder to `%LOCALAPPDATA%\Programs\Composer\bin`. This is automatically upgraded when re-installing over an older version.
* Fixed User uninstall not updating environment.
* Previous-data added: installer version (key: Version)
* Made the repo more collaborator-friendly by moving the exe and code-signing out, re-organizing the code and adding more in-line documentation.
* Moved released versions to Amazon S3 after the demise of Github Downloads.
* Many thanks to [hakre](https://github.com/hakre) for his reports and suggestions.

## v2.6
2012-12-01

* Added proxy support. This checks for an `http_proxy` environment variable and the user's registry settings at *HKCU\Software\Microsoft\Windows\CurrentVersion\Internet Settings*. It tries all the settings it finds, before falling back to a plain request. Sets `http_proxy` locally for install.phar if a proxy was used. Note that authenticated proxies are only supported using an `http_proxy` variable.
* Reworked tests to incorporate proxy support.
* Minor tweaks and doc improvements.

## v2.5
2012-10-29

* Added fix for MsysGit path bug.

## v2.4
2012-10-23

* Added code signing (using Open Source Developer, John Stevenson certificate).

## v2.3
2012-10-23

* Improved path-handling and moved all path functions to a separate file (paths.iss).
* Minor doc tweaks

## v2.2
2012-10-19

* Added functionality for a missing shebang in the output.
* Improved installer captions.

## v2.1
2012-10-18

* Removed Local install option.
* Added uninstaller.
* Added shell script for Cygwin and MsysGit.
* Added Composer logos.
* Minor tweaks, bug fixes and doc improvements.

## v2.0
2012-10-10

* Initial preview release.
