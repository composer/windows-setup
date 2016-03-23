# Composer-Setup

The Windows installer for the [Composer][composer] PHP Dependency Manager. [**Download**][download] the latest release.

## Contents
* [About](#About)
* [Details](#Details)
* [Uninstalling](#Uninstalling)
* [Environment Variables](#Environment)
* [License](#License)

<a name="About"></a>
## About

Composer-Setup downloads and installs [Composer][composer] so you can use it easily from the command line.

* it adds PHP to the path
* it installs Composer globally - just type `composer` from any location to use it
* it can be run from Command Prompt, Git Bash, Msys, Msys2 and Cygwin (including cygwin php)
* it can be installed silently for unattended installations

[Download][download] and run the latest release and in the majority of cases this is all you need to do. The install process is described in more detail below.

Composer-Setup is written with [Inno Setup][inno], which is a powerful free installer for Windows programs.

<a name="Details"></a>
## Details

The first step is to find the location of your `php.exe`, which is the PHP command line interpreter. The installer searches common locations on your computer and presents you with its findings. If PHP is already in your path then it will display this value. If not, or you want to choose a different PHP, you must select from the list or hunt around manually.

The installer will then check that PHP and your path are set up correctly. If it finds any errors it will give you the chance to fix them.

Next, the installer will ask if you need a proxy server to connect to the internet. If it finds any values in your *Internet Settings* or your environment, then the proxy url will be displayed.

The installation directories depends on whether you are an Administrator or not and are non-negotiable.

*Admin install for all users:*

* `C:\<Program Files>\ComposerSetup`
* `C:\ProgramData\ComposerSetup\bin`

*User install:*

* `C:\Users\<user>\AppData\Local\ComposerSetup`
* `C:\Users\<user>\AppData\Local\ComposerSetup\bin`


After you have reviewed and accepted your settings, the installer will download Composer and set everything up. Full usage documentation can be found at [https://getcomposer.org][composer] and a repository of packages can be browsed at [Packagist][packagist].

### Troubleshooting
If you encounter any issues running Composer-Setup, it may help to review the log file it creates. This is stored in your temp directory `AppData\Local\Temp` with a formatted filename: `Setup Log YYYY-MM-DD #nnn.txt`. Note that for Admin installs the log will be deleted on the next reboot.

<a name="Uninstalling"></a>
## Uninstalling

The uninstall program is available from the Control Panel. Go to Programs and Features, select Composer, then click Uninstall. This offers the option to remove cache and configuration data that Composer may have stored.

<a name="Environment"></a>
## Environment Variables

The installer will modify your system path for Admin installs, or your user path for User installs. The installer will add the path to PHP if required and the path to the installation bin directory.

In addition the installer will add the following to the user environment:

* the `COMPOSER_BIN` directory to your path
* an `http_proxy` value, if specified in the installer

The uninstaller will only remove the path to the installation bin directory and the `COMPOSER_BIN` path, leaving the PHP path and any `http_proxy` value intact.

<a name="License"></a>
## License

Composer-Setup is licensed under the MIT License - see the `LICENSE` file for details


  [composer]:   https://getcomposer.org/
  [download]:   https://github.com/johnstevenson/composer-setup/releases/
  [inno]:       http://www.jrsoftware.org/isinfo.php
  [packagist]:  https://packagist.org/
