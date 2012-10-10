<?php

chdir(dirname(__FILE__));
define('CS_SETUP_GUID', '3ECDC245-751A-4962-B580-B8A250EDD1CF');

$status = 2;
$GLOBALS['result'] = '';
csSetupProcess($argv, $status);
exit($status);


function csSetupProcess($argv, &$status)
{

  $phpCheck = in_array('--php', $argv);
  $installCheck = in_array('--install', $argv);

  if ($phpCheck || $installCheck)
  {

    if ($handle = fopen('result.txt', 'wb'))
    {
      $res = $phpCheck ? csSetupPhpCheck() : csSetupInstallCheck('out.txt');
      $status = $res ? 0 : 1;
      fwrite($handle, $GLOBALS['result']);
      fclose($handle);
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

  if (!$errors)
  {
    $GLOBALS['result'] = CS_SETUP_GUID;
    return true;
  }

  $list = array();

  foreach ($errors as $error => $current)
  {

    switch ($error)
    {

      case 'phar':
        $text = "The phar extension is missing.".PHP_EOL;
        $text .= "Install it or recompile php without --disable-phar";
        break;

      case 'unicode':
        $text = "The detect_unicode setting must be disabled.".PHP_EOL;
        $text .= "Add the following to the end of your `php.ini`:".PHP_EOL;
        $text .= "    detect_unicode = Off";
        break;

      case 'php':
        $text = "Your PHP ({$current}) is too old, you must upgrade to PHP 5.3.2 or higher.";
        break;

      case 'allow_url_fopen':
        $text = "The allow_url_fopen setting is incorrect.".PHP_EOL;
        $text .= "Add the following to the end of your `php.ini`:".PHP_EOL;
        $text .= "    allow_url_fopen = On";
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
    $status = 4;
    file_put_contents($filename, $code);
    $argc = 2;
    $argv = array('-', '--', '--quiet');
    eval('?>' . $code);

  }
  else
  {
    $status = $src === $filename ? 2 : 3;
  }

}


function csSetupInstallCheck($filename)
{

  if (!file_exists($filename))
  {
    return false;
  }

  $out = file_get_contents($filename);

  $patterns = array(
    '/\\\\033\\[[0-9]{1,2};[0-9]{2}m/',
    '/\\\\033\\[0m/'
  );

  $out = preg_replace($patterns, '', $out);
  $lines = preg_split("/\r\n|\n\r|\r|\n/", $out);

  if ($lines && strpos($lines[0], '#!/') !== false)
  {
    array_shift($lines);
  }

  $GLOBALS['result'] = trim(implode("\r\n", $lines));

  return true;

}


