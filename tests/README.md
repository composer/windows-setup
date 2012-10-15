# Tests

These are not automated, so the following is a list of manual tests. In most cases these are initiated by running setup in *Test-Mode*, which is enabled with the `/test` command line argument. This displays an *Enter Test* button that allows you to input the test identifier. Note that the command console will show briefly when checking the php settings in test-mode.

## Php Tests

These tests are for checking the input on the *Php Settings* page. They should display the *Php Settings - Error* page with the Next button disabled.

#### Invalid php.exe
* *Setup:* Run test-mode and select a file which isn't php.exe.

* *Expected:* Describes file as not a valid exe file, showing filename.



#### Invalid Version
* *Setup:* Select a version of PHP < 5.3.2

* *Expected:* Describes error and shows current version.


#### Invalid Settings
* *Setup:* Change php.ini, set `allow_url_fopen = off`.

* *Expected:* Describes error and shows the php.ini file used.


#### Invalid ExitCode1
* *Setup:* Run test: `p1`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [201], exit code 2.


#### Invalid ExitCode2
* *Setup:* Run test: `p2`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [201], exit code is `PHP_INT_MAX`.


#### No Result File
* *Setup:* Run test: `p3`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [203], exit code 0


#### Empty Result File
* *Setup:* Run test: `p4`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [204], exit code 0


#### Invalid Indentity in File
* *Setup:* Run test: `p5`.

* *Expected:* Reports that php.exe did not run correctly and shows the filename. Internal Error [205], exit code 0


#### Success but Lines in File
* *Setup:* Run test: `p6`.

* *Expected:* Reports that an internal script did not run correctly. Internal Error [206], exit code 0


#### Failed but No Lines in File
* *Setup:* Run test: `p7`.

* *Expected:* Reports that an internal script did not run correctly. Internal Error [206], exit code 1


## Download Tests
Where files in the *Temp* directory need to be modified, this will usually be found at `~/AppData/Local/Temp` and will be one of two directories prefixed `is-xxx.tmp` where *xxx* is a random string. Generally the tests use test-mode and mimic responses from the install script.


#### Output File Missing
* *Setup:* Put a breakpoint in the `DownloadWork` function on the call `LoadStringsFromFile`, using this pause to delete `install.txt` from the *Temp* directory.

* *Expected:* Reports an internal error [102], with Retry button.



## Path Tests

Not yet implemented

