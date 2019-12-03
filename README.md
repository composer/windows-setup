# Composer-Setup

The Windows installer for the [Composer][composer] PHP Dependency Manager. [**Download**][download] the latest release.

## Contents
* [About](#About)
* [Details](#Details)
* [Php Ini File](#Php-Ini-File)
* [Troubleshooting](#Troubleshooting)
* [Uninstalling](#Uninstalling)
* [Install Locations](#Locations)
* [Environment Variables](#Environment)
* [License](#License)

<a name="About"></a>
## About

Composer-Setup downloads and installs Composer so you can use it easily from the command line.

* it looks for PHP on your computer
* it creates or modifies `php.ini`, if required
* it adds PHP to the path
* it installs Composer globally - just type `composer` from any location to use it
* it lets you use the Command Prompt, Git Bash, Msys, Msys2 and Cygwin (including cygwin php)
* it can be deployed unattended for silent installs/uninstalls

[Download][download] and run the latest release and in the majority of cases this is all you need to do. The install process is described in more detail below.

Composer-Setup is written with [Inno Setup][inno], which is a powerful free installer for Windows programs.

<a name="Details"></a>
## Details

First you must choose the installation type. By default Composer will be installed to a [fixed location](#Locations)
with a Control Panel uninstaller. You can choose _Developer Mode_ if you want more control, which will install
Composer anywhere you want without an uninstaller.

The next step is to find the location of your `php.exe`, which is the PHP command line interpreter. The installer
searches common locations on your computer and presents you with its findings. If PHP is already in your path then it
will display this value. If not, or you want to choose a different PHP, you must select from the list or hunt around manually.

The installer will then check that PHP and your path are set up correctly. If it finds any errors it will give you
the chance to fix them. It will also offer to either create or modify the `php.ini` file if required settings do not exist. See [Php Ini File](#Php-Ini-File) for more information.

Next, the installer will ask if you need a proxy server to connect to the internet. If it finds any values in your
*Internet Settings* or your environment, then the proxy url will be displayed.

After you have reviewed and accepted your settings, the installer will download Composer and set everything up. If your
environment has been changed, it is important to close your current terminal and open a new one so that the changes get
loaded. The installer will remind you if this is the case.

<a name="Php-Ini-File"></a>
## Php Ini File
Composer-Setup will either create a new `php.ini` or modify an existing file if any settings needed by Composer are missing:

* `allow_url_fopen` set to `On`
* `curl`, `mbstring` and `open_ssl` extensions enabled

When creating a new file, the _php.ini-production_ file is used with `extension_dir` and `date.timezone` values appropriately set. When modifying an existing file, a backup is created in the same directory, named `php.ini~orig`.

<a name="Troubleshooting"></a>
## Troubleshooting
Composer-Setup creates a log file, which may be useful to review if you experience any issues. This is stored in your
temp directory `AppData\Local\Temp` with a formatted filename: `Setup Log YYYY-MM-DD #nnn.txt`.

<a name="Uninstalling"></a>
## Uninstalling

The uninstall program is available from the Control Panel. Go to Programs and Features, select Composer, then click
Uninstall. This offers the option to remove user cache and configuration data.

The uninstall program will not be available if you installed in _Developer Mode_. To manually uninstall you must delete
the composer files from the location you installed to and update the [environment](#Environment).

<a name="Locations"></a>
## Install Locations

In a default installation, the install directories are pre-determined and depend on whether you are an Administrator or not.

**Admin install for all users:**

* `C:\<Program Files>\ComposerSetup` - uninstaller
* `C:\ProgramData\ComposerSetup\bin` - composer files

**User install:**

* `C:\Users\<user>\AppData\Local\ComposerSetup` - uninstaller
* `C:\Users\<user>\AppData\Local\ComposerSetup\bin` - composer files

*Note:* These locations are fixed because Composer needs a guaranteed, persistent, writable location to perform its updates.

<a name="Environment"></a>
## Environment Variables

The installer will modify your _system_ path for Admin installs, or your _user_ path for User installs:

* the path to PHP will be added/replaced
* the path to the composer files directory will be added/replaced

In addition the installer will add the following to the _user_ environment:

* the `C:\Users\<user>\AppData\Roaming\Composer\vendor\bin` directory to your path
* an `http_proxy` value, if specified in the installer

The uninstaller will remove the path to the composer files directory and, if the user data was removed, the
`Composer\vendor\bin` path. The PHP path and any `http_proxy` value will be left intact.

<a name="License"></a>
## License

Composer-Setup is licensed under the MIT License - see the `LICENSE` file for details


  [composer]:   https://getcomposer.org/
  [download]:   https://github.com/johnstevenson/composer-setup/releases/latest
  [inno]:       http://www.jrsoftware.org/isinfo.php
