<?php

/**
 * Ini helper for Github Actions
 *
 */

if (!$iniFile = php_ini_loaded_file()) {
    echo 'Cannot find php.ini';
    exit(1);
}

if (in_array('--no-extensions', $argv)) {
    $extensions = ['curl', 'mbstring', 'openssl'];
    foreach ($extensions as $ext) {
        if (extension_loaded($ext)) {
            printf('The %s extension should not be loaded', $ext);
            exit(1);
        }
    }
    exit(0);
}

if (in_array('--no-ini', $argv)) {
    if (!unlink($iniFile)) {
        echo 'Cannot delete php.ini: '.$iniFile;
        exit(1);
    }
    exit(0);
}

if (in_array('--wrong-extdir', $argv)) {
    $data = file_get_contents($iniFile);
    $data = preg_replace('/.*(extension_dir).*/mi', 'extension_dir=/ext', $data);
    $data = preg_replace('/.*(allow_url_fopen).*/mi', '', $data);

    if (!file_put_contents($iniFile, $data)) {
        echo 'Cannot write php.ini: '.$iniFile;
        exit(1);
    }
    exit(0);
}

printf('No known command found: [%s]', implode(',', array_slice($argv, 1)));
exit(1);
