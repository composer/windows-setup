# Composer-Setup

A Windows installer for the Composer PHP Dependency Manager. [Download it here][download].

## Contents
* [About](#About)
* [Usage](#Usage)
* [Uninstalling](#Uninstalling)
* [Compiling](#Compiling)
* [License](#License)

<a name="About"></a>
## About

[Composer][composer] is a wonderful tool but it can sometimes be tricky to install on computers not blessed with a rich development environment coming as standard. Yes, we are looking at you, Windows.

This installer attempts to ease that pain by:

* helping you set up PHP so it can be used from the command line
* checking that your PHP is compatible, no matter how many php.ini files you have lying around
* downloading and installing Composer so that you can use it from any location
* enabling you to run Composer by typing: `composer`

Composer-Setup does **not** write to your registry or do anything unspeakable to your computer other than adding a few files to a folder and updating your path environment variable (but only if you want it to). In fact it is not even a proper installer, in that it has no Uninstall feature, although removal is straightforward and explained [below](#Uninstalling).

> **Tip:** Of course you don't actually need an installer to do any of this. Please see the [Wiki][wiki] for instructions on how to set things up manually or if you are curious.

If you just need Composer in a specific directory, the installer will put the Composer script wherever you want.

Composer-Setup is written using the excellent [Inno Setup][inno] tool, which is a powerful free installer for Windows programs.

<a name="Usage"></a>
## Usage
Download and run [Composer-Setup][download]. You are first asked for the location of your `php.exe`, which is the command line interface to PHP. If this value is in your path variable then it will be displayed for you, otherwise you are going to have to hunt around for it. Depending on your setup it could be:

* In your root `C:\` directory, maybe under `php` or `xampp` or `WampServer` or `EasyPhp`
* In your `C:\Program Files` directory, perhaps under one of the above
* Or wherever else it got installed to

The installer will then check that PHP is set up correctly for Composer. If it finds any errors it will display them and give you the chance to fix them. It will also show the location of the `php.ini` file that is being used, in case of multiple or redundant versions.

Next you have to choose your Installation Type, either Global or Basic. A **Global** installation is recommended because this enables Composer to be run from any location by simply typing `composer`.

A **Basic** installation, on the otherhand, requires you to be in the directory you installed it to and typing `php composer.phar`. This might be suitable in certain circumstances, or if you wish to try out Composer quickly.

After you have chosen the directory you wish to install to and reviewed the choices you have made, the installer will download Composer and set everything up. Full documentation can be found at [http://getcomposer.org][composer] and a repository of packages can be browsed at [Packagist][packagist].

<a name="Uninstalling"></a>
## Uninstalling

There is no Uninstall feature so you will have to do things manually. If you chose the **Basic** installation then you just need to delete `composer.phar` from the directory you installed it to.

If you chose the **Global** installation then you must delete the following files:

* composer.bat
* composer.phar
* composer-README.txt

If the installer added your PHP directory to the path variable, you should only remove it if you no longer want to run PHP from the command line. Your installation directory will have been added to the path variable, but only if it was not already there. remove it only if you are sure that it is not needed by any other programs in this directory.

For a Global install these instructions are in the `composer-README` file in your installation directory.

<a name="Compiling"></a>
## Compiling

To compile the `src/composer.iss` script you will need to install the unicode version of the [Inno Setup Compiler][compiler]. Using the *QuickStart Pack* is the easiest way to install this. Note that the *Encryption* module it not required and neither is the *Inno Setup Preprocessor*, although the latter may change in the future.


<a name="License"></a>
## License

Composer-Setup is licensed under the MIT License - see the `LICENSE` file for details


  [composer]:   http://getcomposer.org
  [download]:   https://github.com/johnstevenson/composer-setup/raw/master/Composer-Setup.exe
  [inno]:       http://www.jrsoftware.org/isinfo.php
  [packagist]:  https://packagist.org/
  [wiki]:       https://github.com/johnstevenson/composer-setup/wiki/Home
  [compiler]:   http://www.jrsoftware.org/isdl.php

