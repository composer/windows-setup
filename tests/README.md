# Tests

These are made up of automated Php Tests, General Tests and manual Installer Tests. To run most of the Installer Tests you must call setup with the `/TEST` command-line argument. This displays an *Enter Test* button that allows you to input the test identifier.

## Php Tests

[![Build Status](https://secure.travis-ci.org/johnstevenson/composer-setup.png)](https://travis-ci.org/johnstevenson/composer-setup)

These test `setup.class.php` which is called by setup to perform php checking and downloading the Composer installer.

## General Tests

These are observational tests that require running setup/uninstall to check for the required behaviour.

#### Usage
* *Setup:* Run setup, open a new command window and run ```composer```
* *Expected:* Composer usage/help information should be displayed

This test must be completed for cmd.exe, Cygwin, Msys and Git Bash, and also for an installation folder with spaces in its name, for example: ```C:\Users\Test User\AppData\Local\Programs\Composer```.

#### Installation Folders
Check that an **Admin** setup installs to (and removes from):

```
C:\ProgramData\Composer
```

<sub>XP: *C:\Documents and Settings\All Users\Application Data\Composer*</sub>

Check that a **User** setup installs to (and removes from):

```
C:\Users\Name\AppData\Local\Programs\Composer
```

<sub>XP: *C:\Documents and Settings\Name\Local Settings\Application Data\Programs\Composer*</sub>


## Installer Php Tests

These tests are for checking the input on the *Settings Check* page. They should display the *Php Settings Error* page with the Next button disabled. Note that the command console will show briefly when checking the php settings.

#### Invalid version
* *Setup:* Select a version of PHP < 5.3.2

* *Expected:* Describes error and shows current version.


#### Invalid settings
* *Setup:* Change php.ini, set `allow_url_fopen = off`.

* *Expected:* Describes error and shows the php.ini file used.


#### Invalid exit code
* *Setup:* Run test: `p1`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [ERR_STATUS], exit code 2.


#### Invalid exit code from php exception
* *Setup:* Run test: `p2`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [ERR_STATUS], exit code is `255`.


#### Empty result file
* *Setup:* Run test: `p3`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [ERR_RESULT], exit code 0


#### Invalid indentity in file
* *Setup:* Run test: `p4`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [ERR_INVALID], exit code 0


#### Success but lines in file
* *Setup:* Run test: `p5`.

* *Expected:* Reports that an internal script did not run correctly. Internal Error [ERR_LOGIC], exit code 0


#### Failed but no lines in file
* *Setup:* Run test: `p6`.

* *Expected:* Reports that an internal script did not run correctly. Internal Error [ERR_LOGIC], exit code 1


## Installer Path Tests

Run the installer/uninstaller with the `/LOG="path\to\log.txt"` command-line argument. The log displays the path before and after any changes.


## Installer Download Tests
The tests generally simulate responses from the install script. These should display the the *Composer Download Error*, except for test `d1` which should show the *Composer Warning* page.


#### Success with a warning
* *Setup:* Run test: `d1`.

* *Expected:* Reports a dummy warning, with a Next button.


#### Failed due to unsuitable Composer php settings
* *Setup:* Run test: `d2`.

* *Expected:* Reports a dummy Composer fatal error, with a disabled Next button


#### Failed due to internal php error
* *Setup:* Run test: `d3`.

* *Expected:* Reports an internal error [ERR_PHP], with a Retry button.


#### Failed connection
* *Setup:* Run test: `d4`, or disconnect from your network.

* *Expected:* Reports a connection error [ERR_CONNECTION], with a Retry button.


#### Success with a proxy warning
* *Setup:* Run test: `d5`.

* *Expected:* Reports warning about internet settings stopping Composer from working, with a Next button.


#### Failed due to unexpected composer response
* *Setup:* Run test: `d6`.

* *Expected:* Reports a Composer error [ERR_STATUS] showing the exit code 15, with a Retry button.


#### Failed, no composer.phar
* *Setup:* Run test: `d7`.

* *Expected:* Reports a Composer error [ERR_DOWNLOAD], with a Retry button.


#### Failed but no Composer errors sent
* *Setup:* Run test: `d8`.

* *Expected:* Reports a Composer error [ERR_INVALID], with a Retry button.
