<?php

/**
* A simple test script that should return a single line of output that
* contains info required by the setup.
*
* The line must start with PHP_CHECK_ID (which is the same value as the
* define in the main install script), followed by pipe (|) separated
* required values and ending with an eol.
*/

$PHP_CHECK_ID = '<ComposerSetup:>';

$tls = (int) extension_loaded('openssl');
$ini = php_ini_loaded_file();
$version = PHP_VERSION;

printf('%s%d|%s|%s%s', $PHP_CHECK_ID, $tls, $ini, $version, PHP_EOL);
exit(0);
