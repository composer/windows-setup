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

    if ($handle = fopen('result.txt', 'wb'))
    {
      $res = csSetupPhpCheck();
      $status = $res ? 0 : 1;
      fwrite($handle, $GLOBALS['result']);
      fclose($handle);
      csSetupTest();
    }

  }
  elseif (in_array('--download', $argv))
  {
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
        $text .= "Add the following to the end of your `php.ini`:".PHP_EOL;
        $text .= "    allow_url_fopen = On";
        break;

      case 'unicode':
        $text = "The detect_unicode setting must be disabled.".PHP_EOL;
        $text .= "Add the following to the end of your `php.ini`:".PHP_EOL;
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

  $GLOBALS['result'] = implode(PHP_EOL.PHP_EOL, $list);
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

  csSetupTest(true);

}


function csSetupGetIdentity()
{
  return CS_SETUP_GUID.PHP_EOL;
}


function csSetupTest($init = false)
{

  if (!$GLOBALS['test'])
  {
    return;
  }

  if ($init)
  {

    switch ($GLOBALS['test'])
    {

      case 'p1':
        exit($GLOBALS['status']);

      case 'p2':
        exit(PHP_INT_MAX);

    }

    return;

  }

  switch ($GLOBALS['test'])
  {

    case 'p3': // delete result file
      @unlink('result.txt');
      break;

    case 'p4': // empty result file
      file_put_contents('result.txt', '');
      break;

    case 'p5': // non matching identity
      file_put_contents('result.txt', 'xxx');
      break;

    case 'p6':
      csSetupTestResult($GLOBALS['test']);
      break;

    case 'p7':
      csSetupTestResult($GLOBALS['test']);
      break;

  }

}


function csSetupTestResult($test)
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

