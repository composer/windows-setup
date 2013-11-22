# Composer-Setup

The Windows installer for the [Composer][composer] PHP Dependency Manager. [**Download it here**][download].

## Contents
* [About](#About)
* [Details](#Details)
* [Changing](#Changing)
* [Uninstalling](#Uninstalling)
* [Path](#Path)
* [Compiling](#Compiling)
* [License](#License)

<a name="About"></a>
## About

[Composer][composer] is a wonderful tool but it can sometimes be tricky to install on Windows. This installer attempts to ease the pain by:

* helping you set up PHP so that it can be used from the command line
* installing Composer so that you can use it from any location by typing: `composer`
* enabling its use from Command Prompt, Git Bash and Cygwin (including cygwin php)
* offering the option to run Composer from Explorer/File Manager windows 

[Download][download] and run it, and in the majority of cases this is all you need to do. The install process is described in more detail below, while the [Wiki][manual] describes how to do it manually.

Composer-Setup is written with [Inno Setup][inno], which is a powerful free installer for Windows programs.

<a name="Details"></a>
## Details

You are first asked if you want to install the optional Shell Menus feature, which allows you to run Composer commands by right-clicking folder items in Windows Explorer/File Manager. You can add or remove this feature at any time - see [Changing](#Changing). Note that the Shell Menus are not available pre Windows Vista. 

Next you are asked for the location of your `php.exe`, which is the PHP command line interpreter. If this value is in your path then it will be displayed, otherwise you are going to have to hunt around. Depending on your setup it could be:

* In your root directory, usually `C:\`, maybe under `php` or `xampp` or `WampServer` or `EasyPhp`
* In your `C:\Program Files` directory, perhaps under one of the above
* Or wherever else it got installed to

The installer will then check that PHP and your path are set up correctly. If it finds any errors it will give you the chance to fix them.

The installation directories depends on whether you are an Administrator or not and are non-negotiable.

*Admin install for all users:*

* `C:\<Program Files>\ComposerSetup` 
* `C:\ProgramData\ComposerSetup\bin`

*User install:*

* `C:\Users\<user>\AppData\Local\ComposerSetup`
* `C:\Users\<user>\AppData\Local\ComposerSetup\bin`


After you have reviewed and accepted your settings, the installer will download Composer and set everything up. Full usage documentation can be found at [http://getcomposer.org][composer] and a repository of packages can be browsed at [Packagist][packagist].

<a name="Changing"></a>
## Changing

You can change/modify your installation at any time from the Control Panel. Go to Programs and Features, select Composer, then click Change. This allows you to re-run setup (perhaps to alter your PHP location) and to either add or remove the Shell Menus feature.

<a name="Uninstalling"></a>
## Uninstalling

The uninstall program is available from the Control Panel. Go to Programs and Features, select Composer, then click Uninstall. This offers the option to remove cache and configuration data that Composer may have stored.

<a name="Path"></a>
## Path

The installer will modify your System Path environment variable for Admin installs, or your User Path environment variable for User installs. The installer will add `php.exe` to the path if required and will also add the path to the installation bin directory. The uninstaller will only remove the path to the bin directory, leaving the PHP path intact.

<a name="Compiling"></a>
## Compiling

To compile the `src/composer.iss` script you will need to install the unicode version of [Inno Setup Compiler][compiler]. Make sure you include *Inno Setup Preprocessor* when you run the installer.


<a name="License"></a>
## License

Composer-Setup is licensed under the MIT License - see the `LICENSE` file for details


  [composer]:   http://getcomposer.org
  [download]:   https://github.com/johnstevenson/composer-setup/releases/download/v2.8/Composer-Setup.2.8.exe
  [inno]:       http://www.jrsoftware.org/isinfo.php
  [packagist]:  https://packagist.org/
  [manual]:     https://github.com/johnstevenson/composer-setup/wiki/Manual-installation
  [compiler]:   http://www.jrsoftware.org/isdl.php
