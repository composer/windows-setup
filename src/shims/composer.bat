@ECHO OFF
SETLOCAL
SET composerScript=composer.phar
php "%~dp0%composerScript%" %*
