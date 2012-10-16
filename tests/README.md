# Tests

These are not automated, so the following is a list of manual tests. In most cases these are initiated by running setup in *Test-Mode*, which is enabled with the `/test` command line argument. This displays an *Enter Test* button that allows you to input the test identifier. Note that the command console will show briefly when checking the php settings in test-mode.

## Php Tests

These tests are for checking the input on the *Php Settings* page. They should display the *Php Settings - Error* page with the Next button disabled.

#### Invalid php.exe
* *Setup:* Run test-mode and select a file which isn't php.exe.

* *Expected:* Describes file as not a valid exe file, showing filename.



#### Invalid version
* *Setup:* Select a version of PHP < 5.3.2

* *Expected:* Describes error and shows current version.


#### Invalid settings
* *Setup:* Change php.ini, set `allow_url_fopen = off`.

* *Expected:* Describes error and shows the php.ini file used.


#### Invalid exit code #1
* *Setup:* Run test: `p1`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [ERR_STATUS], exit code 2.


#### Invalid exit code #2
* *Setup:* Run test: `p2`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [ERR_STATUS], exit code is `PHP_INT_MAX`.


#### No result file
* *Setup:* Run test: `p3`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [ERR_RESULT], exit code 0


#### Empty result file
* *Setup:* Run test: `p4`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [ERR_EMPTY], exit code 0


#### Invalid indentity in file
* *Setup:* Run test: `p5`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [ERR_INVALID], exit code 0


#### Success but lines in file
* *Setup:* Run test: `p6`.

* *Expected:* Reports that an internal script did not run correctly. Internal Error [ERR_LOGIC], exit code 0


#### Failed but no lines in file
* *Setup:* Run test: `p7`.

* *Expected:* Reports that an internal script did not run correctly. Internal Error [ERR_LOGIC], exit code 1


## Path Tests

Not yet implemented


## Download Tests
Generally the tests use test-mode and mimic responses from the install script. To test responses properly it is necessary to grab the downloaded file from the temp directory and modify accordingly. The temp directory will usually be found at `~/AppData/Local/Temp` and will be one of two directories prefixed `is-xxx.tmp` where *xxx* is a random string.


#### Success but no composer.phar
* *Setup:* Run test: `d0`.

* *Expected:* Reports a Composer error [ERR_DOWNLOAD], with Back and Retry buttons.


#### Failed due to php errors
* *Setup:* Run test: `d1`.

* *Expected:* Reports a dummy error, with Back and disabled Next button


#### Failed due to internal php error
* *Setup:* Run test: `d2`.

* *Expected:* Reports an internal error [ERR_PHP], with Back and Retry buttons.


#### Failed connection
* *Setup:* Run test: `d3`, or disconnect from your network.

* *Expected:* Reports a connection error [ERR_CONNECTION], with Back and Retry buttons.


#### Failed due to internal composer error
* *Setup:* Run test: `d4`.

* *Expected:* Reports a Composer error [ERR_INVALID], with Back and Retry buttons.


#### Success with a warning
* *Setup:* Run test: `d5`.

* *Expected:* Reports the warning, with Back and Next buttons.


#### Failed due to unexpected composer response
* *Setup:* Run test: `dn`, where *n* is a number <0 or >5.

* *Expected:* Reports a Composer error [ERR_STATUS], with Back and Retry buttons and the exit code.
