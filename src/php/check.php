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

$version = PHP_VERSION;
$ini = php_ini_loaded_file();
$id = defined('PHP_VERSION_ID') ? PHP_VERSION_ID : 0;
$tls = (int) extension_loaded('openssl');
$cafile = $tls ? trim(ini_get('openssl.cafile')) : '';
$capath = $tls ? trim(ini_get('openssl.capath')) : '';
$compat = $tls ? (int) ini_get('allow_url_fopen') : 0;

$data = array($version, $id, $ini, $tls, $cafile, $capath, $compat);
printf('%s%s%s', $PHP_CHECK_ID, implode('|', $data), PHP_EOL);

exit(0);
