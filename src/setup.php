<?php

chdir(dirname(__FILE__));
define('CS_SETUP_GUID', '3ECDC245-751A-4962-B580-B8A250EDD1CF');

$status = 2;
csSetupProcess($argv, $status);
exit($status);


function csSetupProcess($argv, &$status)
{

  csSetupInit($argv);

  if (in_array('--php', $argv))
  {

    # for testing
    csSetupTestPhp(true);

    if ($handle = fopen('result.txt', 'wb'))
    {
      $res = csSetupPhpCheck();
      $status = $res ? 0 : 1;
      fwrite($handle, $GLOBALS['result']);
      fclose($handle);
    }

    # for testing
    csSetupTestPhp(false);

  }
  elseif (in_array('--download', $argv))
  {

    # for testing
    csSetupTestDownload();

    $force = in_array('--force', $argv);
    csSetupDownload($force, $status);
  }

}


function csSetupPhpCheck()
{

  $errors = array();
  $showIni = false;

  if (version_compare(PHP_VERSION, '5.3.2', '<'))
  {
    $errors['php'] = PHP_VERSION;
  }
  else
  {

    $showIni = true;

    if (!ini_get('allow_url_fopen'))
    {
      $errors['allow_url_fopen'] = true;
    }

    if (ini_get('detect_unicode'))
    {
      $errors['unicode'] = 'On';
    }

    if (!extension_loaded('Phar'))
    {
      $errors['phar'] = true;
    }

  }

  return csSetupFormatResult($errors, $showIni);

}


function csSetupFormatResult($errors, $showIni)
{

  $GLOBALS['result'] = csSetupGetIdentity();

  if (!$errors)
  {
    return true;
  }

  $list = array();

  foreach ($errors as $error => $current)
  {

    switch ($error)
    {

      case 'php':
        $text = "Your PHP ({$current}) is too old, you must upgrade to PHP 5.3.2 or higher.";
        break;

      case 'allow_url_fopen':
        $text = "The allow_url_fopen setting is incorrect.".PHP_EOL;
        $text .= "Add the following to the end of your php.ini:".PHP_EOL;
        $text .= "    allow_url_fopen = On";
        break;

      case 'unicode':
        $text = "The detect_unicode setting must be disabled.".PHP_EOL;
        $text .= "Add the following to the end of your php.ini:".PHP_EOL;
        $text .= "    detect_unicode = Off";
        break;

      case 'phar':
        $text = "The phar extension is missing.".PHP_EOL;
        $text .= "Install it or recompile php without --disable-phar";
        break;
    }

    $list[] = $text;

  }

  if ($showIni)
  {

    $ini = 'Unavailable with this version of PHP';

    if (function_exists('php_ini_loaded_file'))
    {
      $ini = php_ini_loaded_file();
    }

    $list[] = 'php.ini: ' . ($ini ? $ini : 'Not used');

  }

  $GLOBALS['result'] .= implode(PHP_EOL.PHP_EOL, $list);

  return false;

}


function csSetupDownload($force, &$status)
{

  $filename = 'install.phar';

  if (!$force && file_exists($filename))
  {
    $src = 'install.phar';
  }
  else
  {
    $src = (extension_loaded('openssl') ? 'https' : 'http') . '://getcomposer.org/installer';
  }

  if ($code = @file_get_contents($src))
  {

    file_put_contents($filename, $code);
    putenv('ANSICON');
    $argc = 2;
    $argv = array('-', '--', '--quiet');
    eval('?>' . $code);

  }
  else
  {
    $status = $src === $filename ? 2 : 3;
  }

}


function csSetupInit($argv)
{

  $GLOBALS['result'] = '';
  $GLOBALS['test'] = '';

  while ($arg = array_shift($argv))
  {

    if ($arg === '--test')
    {
      $test = array_shift($argv);
      $GLOBALS['test'] = $test ? $test : '';
      break;
    }

  }

}


function csSetupGetIdentity()
{
  return CS_SETUP_GUID.PHP_VERSION.PHP_EOL;
}


# The following functions are for testing

function csSetupTestPhp($before)
{

  if (!$GLOBALS['test'])
  {
    return;
  }

  if ($before)
  {

    switch ($GLOBALS['test'])
    {

      case 'p1':
        exit($GLOBALS['status']);

      case 'p2':
        exit(PHP_INT_MAX);

    }

  }
  else
  {

    switch ($GLOBALS['test'])
    {

      case 'p3': # delete result file
        @unlink('result.txt');
        break;

      case 'p4': # empty result file
        file_put_contents('result.txt', '');
        break;

      case 'p5': # non matching identity
        file_put_contents('result.txt', 'xxx');
        break;

      case 'p6':
        csSetupTestPhpResult($GLOBALS['test']);
        break;

      case 'p7':
        csSetupTestPhpResult($GLOBALS['test']);
        break;

    }

  }

}


function csSetupTestDownload()
{

  if (!$GLOBALS['test'])
  {
    return;
  }

  switch ($GLOBALS['test'])
  {

    case 'd0':
      @unlink('composer.phar');
      exit(0);

    case 'd1':
      echo 'Dummy PHP settings fatal error from installer script'.PHP_EOL;
      exit(1);

    case 'd4':
      exit(1);

    case 'd5':
      echo '#!/usr/bin/env php'.PHP_EOL.PHP_EOL.PHP_EOL.PHP_EOL;
      echo 'Dummy PHP settings warning from installer script'.PHP_EOL;
      file_put_contents('composer.phar', '');
      exit(0);

    case 'd6':
      echo PHP_EOL.PHP_EOL.PHP_EOL.PHP_EOL;
      echo 'Dummy PHP settings warning from installer script'.PHP_EOL;
      file_put_contents('composer.phar', '');
      exit(0);

    default:

      $matches = array();

      if (preg_match('/d(-*\\d{1,})/', $GLOBALS['test'], $matches))
      {
        exit(intval($matches[1]));
      }

  }

}


function csSetupTestPhpResult($test)
{

  /*
    p6 - multiline, status 0
    p7 - first line only, status 1
  */

  $s = csSetupGetIdentity();

  if ($test === 'p6')
  {
    $s .= 'xxx'.PHP_EOL;
  }

  file_put_contents('result.txt', $s);
  $GLOBALS['status'] = $test === 'p6' ? 0 : 1;

}
