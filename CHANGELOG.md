## v2.7

* Added option to delete user data on uninstall.
* Moved User installation directory from roaming folder to ```%LOCALAPPDATA%\Programs\Composer\bin```. This is automatically upgraded when re-installing over an older version.
* Fixed User uninstall not updating environment.
* Made the repo more collaborator-friendly by moving the exe and code-signing out.
* Many thanks to [hakre](https://github.com/hakre) for his reports and suggestions.

## v2.6
2012-12-01 [Composer-Setup.2.6.exe](https://s3-eu-west-1.amazonaws.com/johnstevenson/composer/Composer-Setup.2.6.exe)

* Added proxy support. This checks for an ```http_proxy``` environment variable and the user's registry settings at HKCU\Software\Microsoft\Windows\CurrentVersion\Internet Settings. It tries all the settings it finds, before falling back to a plain request. Sets ```http_proxy``` locally for install.phar if a proxy was used. Note that authenticated proxies are only supported using an ```http_proxy``` variable.

* Reworked tests to incorporate proxy support.
* Minor tweaks and doc improvements.

## v2.5
2012-10-29 [Composer-Setup.2.5.exe](https://s3-eu-west-1.amazonaws.com/johnstevenson/composer/Composer-Setup.2.5.exe)

* Added fix for MsysGit path bug.

## v2.4
2012-10-23 [Composer-Setup.2.4.exe](https://s3-eu-west-1.amazonaws.com/johnstevenson/composer/Composer-Setup.2.4.exe)

* Added code signing (using Open Source Developer, John Stevenson certificate).

## v2.3
2012-10-23 [Composer-Setup.2.3.exe](https://s3-eu-west-1.amazonaws.com/johnstevenson/composer/Composer-Setup.2.3.exe)

* Improved path-handling and moved all path functions to a separate file (paths.iss).
* Minor doc tweaks

## v2.2
2012-10-19 [Composer-Setup.2.2.exe](https://s3-eu-west-1.amazonaws.com/johnstevenson/composer/Composer-Setup.2.2.exe)

* Added functionality for a missing shebang in the output.
* Improved installer captions.

## v2.1
2012-10-18 [Composer-Setup.2.1.exe](https://s3-eu-west-1.amazonaws.com/johnstevenson/composer/Composer-Setup.2.1.exe)

* Removed Local install option.
* Added uninstaller.
* Added shell script for Cygwin and MsysGit.
* Added Composer logos.
* Minor tweaks, bug fixes and doc improvements.

## v2.0
2012-10-10

* Initial preview release.
