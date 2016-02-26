<?php

$guid = $argv[1];
$tls = (int) extension_loaded('openssl');
$ini = php_ini_loaded_file();
$version = PHP_VERSION;

printf('%s%d|%s|%s', $guid, $tls, $ini, $version);
exit(0);
