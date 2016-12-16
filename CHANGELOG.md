## v4.6.0
Not yet released

* Added automatic php.ini creation/modification. A new ini will be offered if one does not already exists, based on the user `php.ini-production` file. If an existing ini is missing required settings, Setup will offer to update it, having created a back-up.
* Added Developer mode install option that allows users to install the Composer specific files where they want to, without including an uninstaller.
* Many code tweaks and improvements.

## v4.5.0
2016-04-14

* Improve welcome page logo.


## v4.4.0
2016-04-14

* Improved search for existing php installations, including Program Files folders.
* Updated installer to use channels.
* Fixed PATHEXT handling bug [issue 54](https://github.com/composer/windows-setup/issues/54).


## v4.3.0
2016-03-24

* Improves path handling for existing user path in admin installs (introduced in v4.2.0) and for adding `Composer\vendor\bin` to the user path.


## v4.2.0
2016-03-24

* Allow an existing user path to be found and used in admin installs. The path is left intact but all other admin-specific locations will be used.


## v4.1.0
2016-03-24

* Major version pushed to v4, reflecting ShellMenus removal which requires uninstalling.
* Updated *userdata.dll* to handle silent uninstalls and removed the dialog cancel button, as it implied rollback functionality. File deletion can always be stopped using the dialog close icon.
* Added `/PHP=path\to\php.ex` and `/PROXY=http_proxy url` params for installation, which can additionally be saved using `/SAFEINF`
and loaded with `/LOADINF`. For uninstall `/DELETE=local` will remove only local data, while `/DELETE=all` will remove all user data.
* Added unattended/silent install and uninstall functionality, with `/SILENT`, `/VERYSILENT` and `/SUPPRESSMSGBOXES` params, as per
Inno Setup documentation.
* Changed Settings page to display a list of existing php installations. The following system drive and user profile directories are searched first: `\php*`, `\php\php*`, `\bin\php*`, `\bin\php\php*`. Then it looks for specific (xampp, wamp, PhpEd) locations.
* Simpler error handling introduced, which recognizes common PHP errors and displays all php output received.
* Added a Proxy page for the user to enter an `http_proxy` value, with a value pre-entered if a proxy is found in any registry Internet
Settings. The variable is then set in the User environment (but not deleted on Uninstall). The Proxy page is also shown if `http_proxy` is already set or passed in as a param.
* Added a Security page for when openssl is disabled, with the option to not use it. Sets `disable-tls` flag for installer script
* Added installation logging, stored as `Setup Log YYYY-MM-DD #nnn.txt` in the users temp directory. For admin installs, the log is deleted on reboot.
* Removed Shell Menus feature to simplify the setup and further development.


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
