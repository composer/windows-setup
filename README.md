# Composer-Setup

A Windows installer for the Composer PHP Dependency Manager. [Download it here][download].

## Contents
* [About](#About)
* [Usage](#Usage)
* [Uninstalling](#Uninstalling)
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

If you just need Composer in a specific directory, the installer will put the Composer script wherever you want. In this case you will have to type `php composer.phar` to run it.

Composer-Setup is written using the excellent [Inno Setup][inno] tool, which is a powerful free installer for Windows programs.

<a name="Usage"></a>
## Usage


<a name="Uninstalling"></a>
## Uninstalling

There is no Uninstall feature so you will have to do things manually, depending on the type of installation you made.

### Basic install
If you chose the Basic installation then you just need to delete `composer.phar` from the directory you installed it to.

### Global install
If you chose the Global installation then you must delete the following files:

* composer.bat
* composer.phar
* composer-README.txt

If the installer added your PHP directory to the path variable, you should only remove it if you no longer want to run PHP from the command line.

Your installation directory will have been added to the path variable, but only if it was not already there. If you are sure that it is not needed by any other programs in this directory, you can remove it .

These instructions can be found in the `composer-README` file.

<a name="License"></a>
## License

Composer-Setup is licensed under the MIT License - see the `LICENSE` file for details


  [composer]: http://getcomposer.org
  [download]: https://github.com/johnstevenson/composer-setup/raw/master/Composer-Setup.exe
  [inno]: http://www.jrsoftware.org/isinfo.php
  [wiki]:https://github.com/johnstevenson/composer-setup/wiki/Home
