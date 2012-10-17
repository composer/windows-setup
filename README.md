# Composer-Setup

A Windows installer for the [Composer][composer] PHP Dependency Manager. [Download it here][download].

## Contents
* [About](#About)
* [Usage](#Usage)
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

Composer-Setup offers two installation types, either Global or Local. Global is recommended and is a *per-user* install process, while Local allows you to just download the main composer script. If you need to setup Composer for all users, or wish to do it manually, full instructions are given in the [Wiki][manual].

Composer-Setup is written with [Inno Setup][inno], which is a powerful free installer for Windows programs.

<a name="Usage"></a>
## Usage
Download and run [Composer-Setup][download]. In the majority of cases this is all you need to do. The section below explains the install process in more detail.


### Details

You are first asked for the location of your `php.exe`, which is the PHP command line interpreter. If this value is in your path then it will be displayed, otherwise you are going to have to hunt around. Depending on your setup it could be:

* In your root directory, usually `C:\`, maybe under `php` or `xampp` or `WampServer` or `EasyPhp`
* In your `C:\Program Files` directory, perhaps under one of the above
* Or wherever else it got installed to

The installer will then check that PHP is set up correctly for Composer. If it finds any errors it will give you the chance to fix them.

Next you have to choose your Installation Type, either Global or Local. A Global installation is recommended because this enables Composer to be run from any location. The files will be installed to `~/AppData/Roaming/Composer/bin` and include an uninstall program.

A Local installation, however, requires you to be in the directory where you installed it and typing `php composer.phar`. This might be suitable in certain circumstances, or if you wish to try out Composer quickly. You can choose the directory that the script is downloaded to.

After you have reviewed your choices, the installer will download Composer and set everything up. Full usage documentation can be found at [http://getcomposer.org][composer] and a repository of packages can be browsed at [Packagist][packagist].

<a name="Uninstalling"></a>
## Uninstalling

The uninstall feature is only included for Global installations and is available from both the Control Panel and your Start Menu.


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

