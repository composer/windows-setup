## v2.8
2013-10-19

* Added support for cygwin php.
* Removed Start menu entry (automatically upgraded when re-installing over an older version)
* Moved released versions back to Github now they have introduced a Releases feature.
* Minor code tweaks.

## v2.7
2012-02-20

* Added an option to delete user data on uninstall. Uses a separate dll (userdata.dll) to get round Inno limitations which provides a cancellable progress form and error report. Only user data found at default locations is deleted. Config entries that point elsewhere are displayed for information but not deleted, and neither is the config.json file. Config entries at project level are not reported. For Admin uninstalls user data is shown for all users, where it can be reliably found.
* Moved User installation directory from roaming folder to ```%LOCALAPPDATA%\Programs\Composer\bin```. This is automatically upgraded when re-installing over an older version.
* Fixed User uninstall not updating environment.
* Made the repo more collaborator-friendly by moving the exe and code-signing out, re-organizing the code and adding more in-line documentation.
* Moved released versions to Amazon S3 after the demise of Github Downloads.
* Many thanks to [hakre](https://github.com/hakre) for his reports and suggestions.

## v2.6
2012-12-01

* Added proxy support. This checks for an ```http_proxy``` environment variable and the user's registry settings at *HKCU\Software\Microsoft\Windows\CurrentVersion\Internet Settings*. It tries all the settings it finds, before falling back to a plain request. Sets ```http_proxy``` locally for install.phar if a proxy was used. Note that authenticated proxies are only supported using an ```http_proxy``` variable.
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
