# Composer Setup

Composer Setup downloads and installs the latest version of Composer, the PHP Dependency Manager, so you can use it easily from the command line: just type `composer` from any location to use it.

Note: The version number refers to the installer and not to Composer, which you can update by running `composer self-update` from your terminal.

##Package Specifics
This package has a dependency on the Chocolatey PHP package. If this is not found, the latest version will be downloaded and installed first.

If you encounter any problems with the installation, you can run it interactively using the `--notsilent` option.

##Advanced Usage
The following package parameters can be set. They are mainly intended for CI usage:

* `/DEV=path` - this installs Composer to the specified path, but without an uninstaller.
* `/PHP=folder-or-exe` - this uses PHP from the specified location, adding it to the path.

These parameters can be passed to the installer by using the `--ia` option.
For example: --ia '"/DEV=C:\tools\php /PHP=C:\php"'.
