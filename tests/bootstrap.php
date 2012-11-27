<?php

ini_set('display_errors', 1);
error_reporting(E_ALL ^ E_NOTICE);

include(realpath('..').'/src/setup.class.php');
include('helpers.php');

spl_autoload_register('autoload');

function autoload($className)
{

  $className = ltrim($className, '\\');
  $fileName  = '';
  $namespace = '';

  if ($lastNsPos = strripos($className, '\\'))
  {
    $namespace = substr($className, 0, $lastNsPos);
    $className = substr($className, $lastNsPos + 1);
    $fileName  = str_replace('\\', DIRECTORY_SEPARATOR, $namespace) . DIRECTORY_SEPARATOR;
  }

  $fileName .= $className . '.php';

  // set the path to our source and test directories, relative to the directory we are in
  $base = realpath('..') . DIRECTORY_SEPARATOR;

  $dirs = array(
    $base . 'src',
    $base . 'tests',
  );

  foreach ($dirs as $path)
  {

    $file = $path . DIRECTORY_SEPARATOR . $fileName;

    if (file_exists($file))
    {
      require $file;
      break;
    }

  }

}
