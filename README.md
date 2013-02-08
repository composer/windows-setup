# Composer-Setup

A Windows installer for the [Composer][composer] PHP Dependency Manager. [**Download it here**][download].

## Contents
* [About](#About)
* [Details](#Details)
* [Uninstalling](#Uninstalling)
* [Path](#Path)
* [Compiling](#Compiling)
* [License](#License)

<a name="About"></a>
## About

[Composer][composer] is a wonderful tool but it can sometimes be tricky to install on Windows. This installer attempts to ease the pain by:

* helping you set up PHP so it can be used from the command line
* installing Composer so that you can use it from any location by typing: `composer`
* setting it up so you can also use it from Git Bash and Cygwin

[Download][download] and run it, and in the majority of cases this is all you need to do. The install process is described in more detail below, while the [Wiki][manual] describes how to do it manually.

Composer-Setup is written with [Inno Setup][inno], which is a powerful free installer for Windows programs.

<a name="Details"></a>
## Details

You are first asked for the location of your `php.exe`, which is the PHP command line interpreter. If this value is in your path then it will be displayed, otherwise you are going to have to hunt around. Depending on your setup it could be:

* In your root directory, usually `C:\`, maybe under `php` or `xampp` or `WampServer` or `EasyPhp`
* In your `C:\Program Files` directory, perhaps under one of the above
* Or wherever else it got installed to

The installer will then check that PHP and your path are set up correctly. If it finds any errors it will give you the chance to fix them.

The installation directory depends on whether you are an Administrator or not and is non-negotiable:

* `%ALLUSERSPROFILE%\Composer\bin` for an Admin install for all users
* `%LOCALAPPDATA%\Programs\Composer\bin` for a User install

After you have accepted these choices, the installer will download Composer and set everything up. Full usage documentation can be found at [http://getcomposer.org][composer] and a repository of packages can be browsed at [Packagist][packagist].

<a name="Uninstalling"></a>
## Uninstalling

The uninstall program is available from the Control Panel. It offers the option to remove cache and configuration data that Composer may have stored.

<a name="Path"></a>
## Path

Composer-Setup will modify your System Path environment variable for Admin installs, or your User Path environment variable for User installs. The installer will add your `php.exe` to the path if required and will also add the path to the installation directory. The uninstaller will only remove the installation directory.

<a name="Compiling"></a>
## Compiling

To compile the `src/composer.iss` script you will need to install the unicode version of [Inno Setup Compiler][compiler]. Using the QuickStart Pack is the easiest way to do this. Note that *Inno Setup Preprocessor* is the only extra that is required.


<a name="License"></a>
## License

Composer-Setup is licensed under the MIT License - see the `LICENSE` file for details


  [composer]:   http://getcomposer.org
  [download]:   https://s3-eu-west-1.amazonaws.com/johnstevenson/composer/Composer-Setup.2.6.exe
  [inno]:       http://www.jrsoftware.org/isinfo.php
  [packagist]:  https://packagist.org/
  [manual]:     https://github.com/johnstevenson/composer-setup/wiki/Manual-installation
  [compiler]:   http://www.jrsoftware.org/isdl.php
