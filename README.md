# Composer-Setup

A Windows installer for the Composer PHP Dependency Manager. [Download it here][download].

## Contents
* [About](#About)
* [Usage](#Usage)
* [License](#License)

<a name="About"></a>
## About

[Composer][composer] is a wonderful tool but it can sometimes be tricky to install on computers not blessed with a rich development environment coming as standard. Yes, we are looking at you, Windows.

This installer attempts to ease that pain by:

* helping you set up PHP so it can be used from the command line
* checking that your PHP is compatible, no matter how many php.ini files you have scattered about
* downloading and installing Composer so that you can use it from any location
* allowing you to run Composer simply by typing `composer`

Composer-Setup does **not** write to your registry or do anything unspeakable to your computer other than adding a few files to a folder and updating your `path` environment variable (but only if you want it to). In fact it is not even a proper installer, in that it has no Uninstall feature.

> **Tip:** Of course you don't actually need an installer to do any of this. Please see the [Wiki][wiki] for instructions on setting this up manually or if you are curious.

Composer-Setup is written using the excellent [Inno Setup][inno] tool, which is a powerful free installer for Windows programs.

<a name="Usage"></a>
## Usage


<a name="License"></a>
## License

Composer-Setup is licensed under the MIT License - see the `LICENSE` file for details


  [composer]: http://getcomposer.org
  [download]: https://github.com/johnstevenson/composer-setup/raw/master/Composer-Setup.exe
  [inno]: http://www.jrsoftware.org/isinfo.php
  [wiki]:https://github.com/johnstevenson/composer-setup/wiki/Home
