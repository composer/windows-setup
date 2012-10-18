# Composer-Setup

A Windows installer for the [Composer][composer] PHP Dependency Manager. [Download it here][download].

## Contents
* [About](#About)
* [Installation](#Installation)
* [Uninstalling](#Uninstalling)
* [Compiling](#Compiling)
* [License](#License)

<a name="About"></a>
## About

[Composer][composer] is a wonderful tool but it can sometimes be tricky to install on computers not blessed with the rich development environment that comes as standard on others. Yes, we are looking at you, Windows.

This installer attempts to ease that pain by:

* helping you set up PHP so it can be used from the command line
* checking that your PHP is compatible
* installing Composer so that you can use it from any location by typing: `composer`
* setting it up so you can also use it from Git Bash and Cygwin

Composer-Setup comes complete with an uninstaller. [Download][download] and run it, and in the majority of cases this is all you need to do. The section below explains the install process in more detail, while the [Wiki][manual] describes how to do it manually.

Composer-Setup is written with [Inno Setup][inno], which is a powerful free installer for Windows programs.

<a name="Installation"></a>
## Installation

You are first asked for the location of your `php.exe`, which is the PHP command line interpreter. If this value is in your path then it will be displayed, otherwise you are going to have to hunt around. Depending on your setup it could be:

* In your root directory, usually `C:\`, maybe under `php` or `xampp` or `WampServer` or `EasyPhp`
* In your `C:\Program Files` directory, perhaps under one of the above
* Or wherever else it got installed to

The installer will then check that PHP and your path are set up correctly. If it finds any errors it will give you the chance to fix them.

The installation directory depends on whether you are an Administrator or not and is non-negotiable:

* `%ALLUSERSPROFILE%\Composer\bin` for an Admin install
* `%APPDATA%\Composer\bin` for a User install

After you have accepted these choices, the installer will download Composer and set everything up. Full usage documentation can be found at [http://getcomposer.org][composer] and a repository of packages can be browsed at [Packagist][packagist].

<a name="Uninstalling"></a>
## Uninstalling

The uninstall program is available from both the Control Panel and the Start Menu.


<a name="Compiling"></a>
## Compiling

To compile the `src/composer.iss` script you will need to install the unicode version of [Inno Setup Compiler][compiler]. Using the QuickStart Pack is the easiest way to do this. Note that *Inno Setup Preprocessor* is the only extra that is required.


<a name="License"></a>
## License

Composer-Setup is licensed under the MIT License - see the `LICENSE` file for details


  [composer]:   http://getcomposer.org
  [download]:   https://github.com/johnstevenson/composer-setup/raw/master/Composer-Setup.exe
  [inno]:       http://www.jrsoftware.org/isinfo.php
  [packagist]:  https://packagist.org/
  [manual]:     https://github.com/johnstevenson/composer-setup/wiki/Manual-installation
  [compiler]:   http://www.jrsoftware.org/isdl.php

