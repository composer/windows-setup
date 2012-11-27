<?php

chdir(dirname(__FILE__));

class ComposerSetup
{

  protected $status;
  protected $debugStr = '';
  protected $switches = array();
  protected $errors = array();

  const CS_SETUP_GUID = '3ECDC245-751A-4962-B580-B8A250EDD1CF';


  /**
  * ComposerSetup constructor.
  *
  * @param Array $argv The arguments passed to the script
  * @param Int $status The exit status code
  * @return ComposerSetup
  */
  public function __construct($argv, &$status)
  {
    $this->init($argv);
    $this->status =& $status;
  }


  /**
  * The main entry point
  *
  */
  public function execute()
  {

    if ($this->getArg('php'))
    {

      # for testing
      $this->setupTestPhp(true);

      $this->status = $this->phpCheck() ? 0 : 1;

      # for testing
      $this->setupTestPhp(false);

    }
    elseif ($this->getArg('download'))
    {

      # for testing
      $this->setupTestDownload();

      $this->download();
    }

  }


  /**
  * Checks various php settings
  *
  */
  protected function phpCheck()
  {

    $this->errors = array();
    $showIni = false;

    echo $this->getIndentity();

    $this->debug('Checking php values', true);

    if (version_compare(PHP_VERSION, '5.3.2', '<'))
    {
      $this->errors['php'] = PHP_VERSION;
    }
    else
    {

      $showIni = true;

      if (!ini_get('allow_url_fopen'))
      {
        $this->errors['allow_url_fopen'] = true;
      }

      if (ini_get('detect_unicode'))
      {
        $this->errors['unicode'] = 'On';
      }

      if (!extension_loaded('Phar'))
      {
        $this->errors['phar'] = true;
      }

    }

    if (!$this->errors)
    {
      $this->debug('success');
      return true;
    }
    else
    {
      $this->debug('errors');
      $this->phpCheckOutputErrors($showIni);
    }

  }


  /**
  * Downloads the installer .phar and evals the contents
  *
  */
  protected function download()
  {

    $filename = dirname(__FILE__).DIRECTORY_SEPARATOR.'install.phar';

    if (!$this->getArg('force') && file_exists($filename))
    {
      # debug
      $this->debug('Download, using existing file: '.$filename);

      $this->install($filename);
      return;
    }

    # debug
    $this->debug('Download, from getcomposer.org/installer');

    $worker = new ComposerSetupRequest();
    $requestList = $worker->get('getcomposer.org/installer', false);

    if ($this->requestsExecute($requestList, $filename))
    {
      $this->install($filename);
    }
    else
    {
      $this->status = 3;
    }

  }


  protected function requestsExecute($requestList, $filename)
  {

    $result = false;
    $defTimeout = ini_get('default_socket_timeout');
    $errors = array();

    set_error_handler(array($this, 'errorHandler'));

    foreach ($requestList as $request)
    {

      if ($request['error'])
      {
        #debug
        $this->debug($request['error']);

        $errors[] = $request['error'];
        continue;
      }

      $msg = 'Request to '.$request['source'];
      $msg .= $request['proxy'] ? ', proxy: '.$request['display'] : '';

      # debug
      $this->debug($msg, true);

      # set shorter timeout for ssl proxy
      ini_set('default_socket_timeout', $request['ssl'] ? '10' : '20');

      # make the request
      if ($result = copy($request['source'], $filename, $request['context']))
      {
        break;
      }
      else
      {
        # debug
        $this->debug('failed');

        if ($errRequest = $this->requestGetErrors())
        {
          # debug
          $this->debug($errRequest);

          $errors[] = $errRequest;
        }

      }

    }

    restore_error_handler();
    ini_set('default_socket_timeout', $defTimeout);

    if ($result)
    {

      # debug
      $this->debug('success');

      # set http_proxy for install script
      if ($request['proxy'])
      {
        $this->debug('Adding proxy value to local environment: '.$request['display']);
        $_SERVER['http_proxy'] = $request['proxy'];
        $_SERVER['HTTP_PROXY'] = $request['proxy'];
      }

    }
    else
    {
      echo implode(PHP_EOL.PHP_EOL, $errors);
    }

    return $result;

  }


  /**
  * Empties errors from each request
  *
  */
  protected function requestGetErrors()
  {
    $errorStr = implode(PHP_EOL.PHP_EOL, $this->errors);
    $this->errors = array();
    return $errorStr;
  }


  public function errorHandler($code, $msg)
  {
    $this->errors[] = ucfirst(preg_replace('/^copy\(.*?\): /', '', $msg));
    return true;
  }


  public function debug($msg, $part = false)
  {

    if ($this->getArg('debug'))
    {

      $msg = str_replace(PHP_EOL, ' ', $msg);

      if (!$part && $this->debugStr)
      {
        $msg = $this->debugStr.$msg;
        $this->debugStr = '';
      }
      elseif ($part)
      {
        $this->debugStr = $msg.', ';
        $msg = '';
      }

      if ($msg)
      {
        echo self::CS_SETUP_GUID.$msg.PHP_EOL;
      }

    }

  }


  /**
  * Includes the downloaded .phar to carry out the installation
  *
  * @param String $filename The downloaded .phar
  */
  protected function install($filename)
  {

    $GLOBALS['csStatus'] = 0;

    putenv('ANSICON');
    $argc = 2;
    $argv = array('-', '--', '--quiet');
    include($filename);

  }


  /**
  * Parses script arguments
  *
  * @param Array $argv The arguments passed to the script
  */
  protected function init($argv)
  {


    $lastKey = '';

    foreach ($argv as $arg)
    {

      if (strpos($arg, '--') === 0)
      {
        $lastKey = substr($arg, 2);
        $this->switches[$lastKey] = '';
      }
      elseif ($lastKey)
      {
        $this->switches[$lastKey] = $arg;
      }

    }

  }

  /**
  * Returns whether an argument has been passed in, or its value.
  *
  *
  *
  * @param string $name The name of the argument switch
  * @param bool $value Whether the argument should have a value
  */
  protected function getArg($name, $value = false)
  {

    if (isset($this->switches[$name]))
    {
      return $value ? $this->switches[$name] : true;
    }

  }


  /**
  * Returns a unique identifier
  *
  */
  protected function getIndentity()
  {
    return self::CS_SETUP_GUID.PHP_VERSION.PHP_EOL;
  }


  /**
  * Formats any errors
  *
  * @param bool $showIni Whether to get php.ini location
  */
  protected function phpCheckOutputErrors($showIni)
  {

    $list = array();

    foreach ($this->errors as $error => $current)
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

    echo implode(PHP_EOL.PHP_EOL, $list);

  }


  protected function setupTestPhp($before)
  {

    if (!$test = $this->getArg('test', true))
    {
      return;
    }

    if ($before)
    {

      ob_start();

      switch ($test)
      {

        case 'p1':
          exit($this->status);

        case 'p2':
          throw new Exception('Dummy');

        case 'p3':
          exit(PHP_INT_MAX);

      }

    }
    else
    {

      ob_end_clean();

      switch ($test)
      {

        case 'p4': # empty result file
          exit(0);

        case 'p5': # non matching identity
          echo 'xxx'.PHP_EOL;
          exit(0);

        case 'p6':
          $this->setupTestPhpResult($test);
          break;

        case 'p7':
          $this->setupTestPhpResult($test);
          break;

      }

    }

  }


  protected function setupTestPhpResult($test)
  {

    /*
      p6 - multiline, status 0
      p7 - first line only, status 1
    */

    echo $this->getIndentity();

    if ($test === 'p6')
    {
      echo 'xxx'.PHP_EOL;
    }

    $this->status = $test === 'p6' ? 0 : 1;

  }


  protected function setupTestDownload()
  {

    if (!$test = $this->getArg('test', true))
    {
      return;
    }

    switch ($test)
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

        if (preg_match('/d(-*\\d{1,})/', $test, $matches))
        {
          exit(intval($matches[1]));
        }

    }

  }


}


class ComposerSetupRequest
{

  protected $scheme = '';


  /**
  * Setting scheme in constructor allows us to test
  *
  */
  public function __construct()
  {
    $this->scheme = extension_loaded('openssl') ? 'https' : 'http';
  }


  /**
  * Populates and returns requests array
  *
  */
  public function get($url, $strict = true)
  {

    $requests = array();
    $url = preg_replace('/https?:\/\//i', '', $url);
    $strict = $strict;

    # get proxy from environment variables

    if ($params = $this->proxyFromEnv())
    {
      $requests = $params;
    }

    # get proxy from registry
    if (!$requests || !$strict)
    {

      if ($params = $this->proxyFromReg())
      {
        $requests = array_merge($requests, $params);
      }

    }

    # add default non-proxy last
    if (!$requests || !$strict)
    {
      $item = $this->param();
      $item['scheme'] = $this->scheme;
      $requests[] = $item;
    }

    # populate source and context fields
    foreach ($requests as &$params)
    {

      $params['source'] = $params['scheme'].'://'.$url;
      unset($params['scheme']);

      if (!$params['error'])
      {
        $options['http'] = $params['http'];
        $params['context'] = stream_context_create($options);
      }

      unset($params['http']);

    }

    return $requests;

  }


  protected function proxyFromEnv()
  {

    if ($list = $this->envGetProxies($_SERVER))
    {
      return $this->proxyProcess($list, false);
    }

  }


  /**
  * reads the registry for proxy information for current user
  *
  */
  protected function proxyFromReg()
  {

    $key = 'HKCU\Software\Microsoft\Windows\CurrentVersion\Internet Settings';

    # ProxyEnable
    if (!$this->winRegGet($key, 'ProxyEnable', $value) || !$value)
    {
      return;
    }

    # ProxyOverride
    if ($this->winRegGet($key, 'ProxyOverride', $value) && $value)
    {

      if (false !== stripos($value, 'getcomposer.org'))
      {
        return;
      }

    }

    # ProxyServer
    if ($this->winRegGet($key, 'ProxyServer', $value) && $value)
    {

      if ($list = $this->regGetProxies($value))
      {
        return $this->proxyProcess($list, true);
      }

    }

  }


  protected function proxyProcess($list, $fromReg)
  {

    $fmtError = 'Cannot use proxy %s from %s. %s';
    $fmtDisplay = '%s from %s';
    $from = $fromReg ? 'Internet Settings' : 'environment variable';
    $sslError = 'You must enable the openssl extension to use a proxy over https';

    $checked = array();

    foreach ($list as $key => $item)
    {

      if ($item)
      {
        $options = array();
        $this->proxyGetOptions($item, $options);

        if ($options['error'])
        {
          $options['error'] = sprintf($fmtError, $options['display'], $from, $options['error']);
        }
        elseif ($options['ssl'] && 'http' === $scheme)
        {
          $options['error'] = sprintf($fmtError, $options['display'], $from, $sslError);
        }

        $options['display'] = sprintf($fmtDisplay, $options['display'], $from);
        $checked[$key] = $options;
      }

    }

    if (!$first = isset($checked[$this->scheme]) ? $checked[$this->scheme] : null)
    {
      return;
    }

    $other = 'https' === $this->scheme ? 'http' : 'https';

    if ($second = isset($checked[$other]) ? $checked[$other] : null)
    {

      if ($first['proxy'] === $second['proxy'])
      {
        unset($checked[$other]);
      }

    }

    $out = array();

    foreach ($checked as $scheme => $options)
    {
      $options['scheme'] = $scheme;
      $out[] = $options;
    }

    return $out;

  }


  protected function proxyGetOptions($proxyStr, &$options)
  {

    $proxyStr = trim($proxyStr);

    $pattern = '/:[^\/](.|\s)*?@/';
    $replace = ':*****@';

    $options = $this->param();
    $options['proxy'] = $proxyStr;
    $options['display'] = preg_replace($pattern, $replace, $proxyStr, 1);
    $options['ssl'] = false;

    # check for UNC name
    if (0 === strpos($proxyStr, '\\'))
    {
      $options['error'] = 'UNC proxy name not supported.';
      return;
    }
    elseif (false === strpos($proxyStr, '://'))
    {
      $proxyStr = 'dummy://' . $proxyStr;
    }

    # fail if we cannot parse the url
    if (!$proxy = parse_url($proxyStr))
    {
      $options['error'] = 'Invalid proxy value.';
      return;
    }

    # get/set default values
    $proxy['scheme'] = isset($proxy['scheme']) ? strtolower(trim($proxy['scheme'])) : '';
    $proxy['host'] = isset($proxy['host']) ? strtolower(trim($proxy['host'])) : '';
    $proxy['port'] = isset($proxy['port']) ? trim($proxy['port']) : '';
    $proxy['user'] = isset($proxy['user']) ? trim($proxy['user']) : '';
    $proxy['pass'] = isset($proxy['pass']) ? $proxy['pass'] : '';

    # fail if scheme or host are missing
    if (!$proxy['scheme'] || !$proxy['host'])
    {
      $options['error'] = 'Missing proxy value.';
      return;
    }

    # fail if scheme not one of http, https, tcp, ssl, dummy
    if (!in_array($proxy['scheme'], array('http', 'https', 'tcp', 'ssl', 'dummy')))
    {
      $options['error'] = 'Unknown proxy scheme.';
      return;
    }

    # add scheme where missing, according to port
    if ($proxy['scheme'] === 'dummy')
    {

      if ('80' === $proxy['port'])
      {
        $proxy['scheme'] = 'http';
      }
      elseif ('443' === $proxy['port'])
      {
        $proxy['scheme'] = 'https';
      }
      else
      {
        $proxy['scheme'] = 'http';
        $proxy['port'] = $proxy['port'] ? $proxy['port'] : '80';
      }

    }
    elseif (!$proxy['port'])
    {

      # add port where missing, according to scheme
      if ('http' === $proxy['scheme'])
      {
        $proxy['port'] = '80';
      }
      elseif ('https' === $proxy['scheme'])
      {
        $proxy['port'] = '443';
      }
      else
      {
        $options['error'] = 'Missing proxy port value.';
        return;
      }

    }

    $proxyURL = $proxy['scheme'] . '://' . $proxy['host'] . ':' . $proxy['port'];
    $proxyURL = str_replace(array('http://', 'https://'), array('tcp://', 'ssl://'), $proxyURL);
    $options['ssl'] = strpos($proxyURL, 'ssl://') === 0;

    $options['http'] = array(
      'proxy' => $proxyURL,
      'request_fulluri' => true,
    );

    if ($proxy['user'])
    {
      $auth = $proxy['user'] . ':' . $proxy['pass'];
      $options['http']['header'] = 'Proxy-Authorization: Basic ' . base64_encode($auth);
      $proxyURL = str_replace('://', '://'.$auth.'@', $proxyURL);
    }

    $options['proxy'] = $proxyURL;
    $options['display'] = preg_replace($pattern, $replace, $proxyURL, 1);

  }


  protected function envGetProxies($env)
  {

    $env = (array) $env;
    $list = array();

    foreach ($env as $key => $value)
    {

      if ('http_proxy' === strtolower($key))
      {
        $list['http'] = $value;
      }
      elseif ('https_proxy' === strtolower($key))
      {
        $list['https'] = $value;
      }

    }

    if ($list && !isset($list['https']))
    {
      $list['https'] = $list['http'];
    }

    return $list;

  }


  protected function regGetProxies($proxyStr)
  {

    $proxyStr = (string) $proxyStr;

    # remove errant = and trailing separators (some browsers do this)
    $proxyStr = trim(trim($proxyStr), '=;');

    if (!$proxyStr)
    {
      return;
    }

    $list = array();

    if (false === strpos($proxyStr, '='))
    {
      $list['http'] = $proxyStr;
      $list['https'] = $proxyStr;
    }
    else
    {

      $entries = explode(';', $proxyStr);

      foreach ($entries as $item)
      {

        $parts = explode('=', $item, 2);
        $protocol = strtolower(trim($parts[0]));
        $value = trim($parts[1]);

        if (2 === count($parts) && ('http' === $protocol || 'https' === $protocol))
        {
          # we only take the first found value
          if (!isset($list[$protocol]) && $value)
          {
            $list[$protocol] = $value;
          }
        }

      }

    }

    return $list;

  }


  /**
  * Reads a value from the registry, using Windows reg.exe
  *
  * Returns an Array of output lines or null
  *
  * @param String $key The parent key of the value
  * @param String $name The name of the value
  */
  protected function winRegRead($key, $name)
  {

    if ('WIN' === strtoupper(substr(PHP_OS, 0, 3)))
    {

      $cmd = 'reg query "'.$key.'" /v "'.$name.'"';
      exec($cmd . ' 2>&1', $output, $status);

      if ($status === 0)
      {
        return $output;
      }

    }

  }


  /**
  * Reads a value from the registry
  *
  * Returns True if value read from registry, otherwise null
  *
  * @param String $key The parent key of the value
  * @param String $name The name of the value
  * @param mixed $value The returned value
  * @param Bool $raw Whether to format REG_DWORD, QWORD, BINARY and MULTI_SZ values
  */
  protected function winRegGet($key, $name, &$value, $raw = false)
  {

    $value = null;

    if (!$output = $this->winRegRead($key, $name))
    {
      return;
    }

    $line = implode('', $output);

    if ($pos = stripos($line, $name))
    {

      $line = trim(substr($line, $pos + strlen($name)));
      $parts = preg_split('/\s+/', $line, 2);

      if (count($parts) === 2)
      {

        $type =  strtoupper($parts[0]);
        $value = $parts[1];

        if ('REG_DWORD' === $type || 'REG_QWORD' === $type)
        {
          $value = $raw ? $value : intval($value, 16);
        }
        elseif ('REG_BINARY' === $type)
        {
          $value = $raw ? $value : @pack("H*" , $value);
        }
        elseif ('REG_MULTI_SZ' === $type)
        {
          $value = $raw ? $value : explode('\0', $value);
        }

      }

      return true;

    }

  }


  protected function param()
  {

    return array(
      'source' => '',
      'context' => null,
      'scheme' => '',
      'http' => array(),
      'proxy' => '',
      'display' => '',
      'ssl' => false,
      'error' => '',
    );

  }

}
