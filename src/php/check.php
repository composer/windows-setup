<?php

/**
* A simple test script that should return a single line of output that
* contains info required by the setup.
*
* The line must start with PHP_CHECK_ID (which is the same value as the
* define in the main install script), followed by pipe (|) separated
* required values and ending with an eol. Note that the last value MUST
* not be an empty string.
*/

$PHP_CHECK_ID = '<ComposerSetup:>';

$version = PHP_VERSION;
$id = defined('PHP_VERSION_ID') ? PHP_VERSION_ID : 0;
$ini = php_ini_loaded_file();
$other = defined('PHP_WINDOWS_VERSION_BUILD') ? '' : PHP_OS;
$compat = $id && $ini && !$other;

// Core ini values
$fopen = (bool) ini_get('allow_url_fopen');
$timezone = PHP_MAJOR_VERSION < 7 ? (bool) ini_get('date.timezone') : true;
$compat = $compat && $fopen && $timezone;

// Required extensions
$curl = extension_loaded('curl');
$mbstring = extension_loaded('mbstring');
$openssl = extension_loaded('openssl');
$compat = (int) ($compat && $curl && $mbstring && $openssl);

// CA info to help with connection issues
$cafile = $openssl ? trim(ini_get('openssl.cafile')) : '';
$capath = $openssl ? trim(ini_get('openssl.capath')) : '';

$info = array();

if (!$ini) {
    $info[] = 'extension_dir = "ext"';
}

if (!$fopen) {
    $info[] = 'allow_url_fopen = On';
}

if (!$timezone) {
    $info[] = 'date.timezone = UTC';
}

$exts = array('curl' => $curl, 'mbstring' => $mbstring, 'openssl' => $openssl);
$fullname = version_compare(PHP_VERSION, '7.2.0', '<');

foreach ($exts as $name => $loaded) {
    if (!$loaded) {
        $info[] = 'extension='.($fullname ? 'php_'.$name.'.dll' : $name);
    }
}

$missing = implode(',', $info);
$data = array($version, $id, $ini, $other, $cafile, $capath, $missing, $compat);
printf('%s%s%s', $PHP_CHECK_ID, implode('|', $data), PHP_EOL);

exit(0);
