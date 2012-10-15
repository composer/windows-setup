# Tests

These are not automated, so the following is a list of manual tests.

## Php Tests

These tests are for checking the input on the *Php Settings* page. They should display the *Php Settings - Error* page with the Next button disabled.

### Invalid php.exe Test
**Setup** Run with /test=[anything] and select a file which isn't php.exe.

**Expected** Describes file as not a valid exe file, showing filename.


### Invalid Version Test
**Setup** Select a version of PHP < 5.3.2

**Expected** Describes error and shows current version.


### Invalid Settings Test
**Setup** Change php.ini, set `allow_url_fopen = off`.

**Expected** Describes error and shows the php.ini file used.


### Invalid ExitCode1
**Setup** Run with `/test=p1`.

**Expected** Reports that php.exe did not run correctly and shows the filename. Internal Error [201], exit code 2.


### Invalid ExitCode2
**Setup** Run with `/test=p2`.

**Expected** Reports that php.exe did not run correctly and shows the filename. Internal Error [201], exit code is `PHP_INT_MAX`.


### No Result File
**Setup** Run with `/test=p3`.

**Expected** Reports that php.exe did not run correctly and shows the filename. Internal Error [203], exit code 0


### Empty Result File
**Setup** Run with `/test=p4`.

**Expected** Reports that php.exe did not run correctly and shows the filename. Internal Error [204], exit code 0


### Invalid Indentity in File
**Setup** Run with `/test=p5`.

**Expected** Reports that php.exe did not run correctly and shows the filename. Internal Error [205], exit code 0


### Success but Lines in File
**Setup** Run with `/test=p6`.

**Expected** Reports that an internal script did not run correctly. Internal Error [206], exit code 0


### Failed but No Lines in File
**Setup** Run with `/test=p6`.

**Expected** Reports that an internal script did not run correctly. Internal Error [206], exit code 1


## Path Tests

These tests are

