<?php

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
      $this->debug('Download, using existing file: '.$filename);
      $this->install($filename);
      return;
    }

    $url = 'getcomposer.org/installer';
    $this->debug('Download, from '.$url);

    $broker = new ComposerSetupRequestBroker();
    $requestList = $broker->getRequests($url, false);

    if ($this->requestsExecute($requestList, $filename))
    {
      $this->debug('Download ok, running install');
      $this->install($filename);
    }
    else
    {
      $this->debug('Download failed');
      $this->status = 3;
    }

  }


  protected function requestsExecute($requestList, $filename)
  {

    $result = false;
    $errors = array();

    $defTimeout = ini_set('default_socket_timeout', '15');
    set_error_handler(array($this, 'errorHandler'));

    foreach ($requestList as $request)
    {

      $this->debug($request->msg);

      if (!$request->context)
      {
        $errors[] = $request->msg;
        continue;
      }

      # make the request
      if ($result = copy($request->source, $filename, $request->context))
      {
        $this->debug('Request ok');
        break;
      }
      else
      {
        $errRequest = $this->requestGetErrors();
        $this->debug('Failed with errors: ' .$errRequest);

        if ($request->proxyUrl)
        {
          $errMsg = 'Proxy '.$request->proxyDisplay;
        }
        else
        {
          $errMsg = $request->msg;
        }

        $errors[] = $errMsg.' failed with errors:'.PHP_EOL.$errRequest;
      }

    }

    restore_error_handler();
    ini_set('default_socket_timeout', $defTimeout);

    if ($result)
    {

      # set http_proxy for install script
      if ($request->proxyUrl)
      {
        $this->debug('Adding proxy value to local environment: '.$request->proxyDisplay);
        $_SERVER['http_proxy'] = $request->proxyUrl;
        $_SERVER['HTTP_PROXY'] = $request->proxyUrl;
      }

      if ($errors)
      {
        array_unshift($errors, 'Warning: Your internet settings may stop Composer from working.');
      }

    }

    if ($errors)
    {
      echo implode(PHP_EOL.PHP_EOL, $errors).PHP_EOL;
    }

    return $result;

  }


  /**
  * Empties errors from each request, formats and returns them
  *
  */
  protected function requestGetErrors()
  {
    $s = implode('. ', $this->errors);
    $s = str_replace('..', '.', $s);

    $this->errors = array();
    return $s ? $s : 'no errors reported';

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

    if ((!$test = $this->getArg('test', true)) || $test[0] !== 'p')
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

      }

    }
    else
    {

      ob_end_clean();

      switch ($test)
      {

        case 'p3': # empty result file
          exit(0);

        case 'p4': # non matching identity
          echo 'xxx'.PHP_EOL;
          exit(0);

        case 'p5':
          $this->setupTestPhpResult($test);
          break;

        case 'p6':
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

    if ($test === 'p5')
    {
      echo 'xxx'.PHP_EOL;
    }

    $this->status = $test === 'p5' ? 0 : 1;

  }


  protected function setupTestDownload()
  {

    if ((!$test = $this->getArg('test', true)) || $test[0] !== 'd')
    {
      return;
    }

    switch ($test)
    {

      case 'd1':
        # [ERR_NONE]
        $this->debug('This message should not be shown');
        echo PHP_EOL.PHP_EOL.'#!/usr/bin/env php'.PHP_EOL.PHP_EOL.PHP_EOL.PHP_EOL;
        echo 'Dummy PHP settings warning from installer script'.PHP_EOL;
        file_put_contents('composer.phar', '');
        exit(0);

      case 'd2':
        # [ERR_INSTALL]
        echo 'Dummy Composer fatal error from installer script'.PHP_EOL;
        exit(1);

      case 'd3':
        # [ERR_PHP]
        exit(2);

      case 'd4':
        # [ERR_CONNECTION]
        exit(3);

      case 'd5':
        # [ERR_CONNECTION]
        $_SERVER['http_proxy'] = 'http://127.0.0.100:28000';
        return;

      case 'd6':
        # [ERR_STATUS]
        exit(15);

      case 'd7':
        # [ERR_DOWNLOAD]
        @unlink('composer.phar');
        exit(0);

      case 'd8':
        # [ERR_INVALID]
        exit(1);

    }

  }


}


class ComposerSetupRequestBroker
{

  protected $scheme = '';
  protected $winRegistry;

  const PROXY_SRC_ENV = 'Environment Variables';
  const PROXY_SRC_REG = 'Internet Settings';
  const PROXY_ERR_UNC = 'UNC proxy name not supported.';
  const PROXY_ERR_INVALID = 'Invalid proxy value.';
  const PROXY_ERR_MISSING = 'Missing proxy value.';
  const PROXY_ERR_UNKNOWN = 'Unknown proxy scheme.';
  const PROXY_ERR_PORT = 'Missing proxy port value.';
  const PROXY_ERR_SSL = 'You must enable the openssl extension to use a proxy over https.';


  /**
  * Setting stuff in constructor allows us to test
  *
  */
  public function __construct()
  {
    $this->scheme = extension_loaded('openssl') ? 'https' : 'http';

    if ('WIN' === strtoupper(substr(PHP_OS, 0, 3)))
    {
      $this->winRegistry = new ComposerSetupWindowsRegistry();
    }

  }


  /**
  * Populates and returns an array of ComposerSetupRequest objects
  *
  * @param String $url
  * @param Bool $strict Whether to return non-proxy if proxy found
  */
  public function getRequests($url, $strict = true)
  {

    $items = array();
    $url = preg_replace('/https?:\/\//i', '', $url);

    # get proxy from environment variables
    if ($params = $this->proxyFromEnvironment())
    {
      $items = $params;
    }

    # get proxy from registry
    if ($this->winRegistry && (!$items || !$strict))
    {

      if ($params = $this->proxyFromRegistry($url))
      {
        $items = array_merge($items, $params);
      }

    }

    # add default non-proxy last
    if (!$items || !$strict)
    {
      $params = $this->proxyParams();
      $params['scheme'] = $this->scheme;
      $items[] = $params;
    }

    return $this->formatRequests($items, $url);

  }


  /**
  * Creates an array of ComposerSetupRequest objects
  *
  * @param Array $list
  * @param String $url
  */
  protected function formatRequests($list, $url)
  {

    $out = array();

    foreach ($list as $item)
    {

      $request = new ComposerSetupRequest();
      $request->source = $item['scheme'].'://'.$url;

      if ($item['proxyError'])
      {
        $request->context = null;
        $request->msg = $item['proxyError'];
      }
      else
      {
        $request->context = stream_context_create(array('http' => $item['http']));
        $request->msg = 'Request to '.$request->source;
        $request->msg .= $item['proxyUrl'] ? ', using proxy '.$item['proxyDisplay'] : '';
      }

      $request->proxyUrl = $item['proxyUrl'];
      $request->proxyDisplay = $item['proxyDisplay'];

      $out[] = $request;

    }

    return $out;

  }


  /**
  * reads environment variables for proxy information
  *
  */
  protected function proxyFromEnvironment()
  {

    if ($list = $this->proxyGetEnvironment($_SERVER))
    {
      return $this->proxyProcess($list, false);
    }

  }


  /**
  * reads the registry for proxy information
  *
  * @param String $url The host/path of the request url
  */
  protected function proxyFromRegistry($url)
  {

    if ($proxyStr = $this->proxyReadRegistry($url))
    {

      if ($list = $this->proxyGetRegistry($proxyStr))
      {
        return $this->proxyProcess($list, true);
      }

    }

  }


  /**
  * reads the registry for proxy information for current user
  *
  * @param String $url The request url
  */
  protected function proxyReadRegistry($url)
  {

    $key = 'HKCU\Software\Microsoft\Windows\CurrentVersion\Internet Settings';
    $url = parse_url($url, PHP_URL_HOST);

    # ProxyEnable
    if (!$this->winRegistry->read($key, 'ProxyEnable', $value) || !$value)
    {
      return;
    }

    # ProxyOverride
    if ($this->winRegistry->read($key, 'ProxyOverride', $value) && $value)
    {

      if ($url && false !== stripos($value, $url))
      {
        return;
      }

    }

    # ProxyServer
    if ($this->winRegistry->read($key, 'ProxyServer', $value) && $value)
    {
      return $value;
    }

  }


  protected function proxyProcess($list, $fromReg)
  {

    $out = array();

    $params = $this->proxyGetParams($list, $fromReg);

    if (!$first = isset($params[$this->scheme]) ? $params[$this->scheme] : null)
    {
      return;
    }

    $other = 'https' === $this->scheme ? 'http' : 'https';

    if ($second = isset($params[$other]) ? $params[$other] : null)
    {

      if ($first['proxyUrl'] === $second['proxyUrl'] || 'http' === $this->scheme)
      {
        unset($params[$other]);
        $second = null;
      }

    }

    $first['scheme'] = $this->scheme;
    $out[] = $first;

    if ($second)
    {
      $second ['scheme'] = $other;
      $out[] = $second;
    }

    return $out;

  }


  protected function proxyGetParams($list, $fromReg)
  {

    $fmtDisplay = '%s [from %s]';
    $fmtError = 'Cannot use proxy %s. %s';
    $from = $fromReg ? self::PROXY_SRC_REG : self::PROXY_SRC_ENV;

    $out = array();

    foreach ($list as $key => $proxyStr)
    {

      if ($proxyStr)
      {
        $this->proxySetParams($proxyStr, $params);

        $display = preg_replace('/:[^\/](.|\s)*?@/', ':*****@', $params['proxyUrl'], 1);
        $params['proxyDisplay'] = sprintf($fmtDisplay, $display, $from);

        if ($params['proxyError'])
        {
          $params['proxyError'] = sprintf($fmtError, $params['proxyDisplay'], $params['proxyError']);
        }

        $out[$key] = $params;
      }

    }

    return $out;

  }


  protected function proxySetParams($proxyStr, &$params)
  {

    $proxyStr = trim($proxyStr);

    $params = $this->proxyParams();
    $params['proxyUrl'] = $proxyStr;

    # check for UNC name
    if (0 === strpos($proxyStr, '\\\\'))
    {
      $params['proxyError'] = self::PROXY_ERR_UNC;
      return;
    }
    elseif (false === strpos($proxyStr, '://'))
    {
      $proxyStr = 'dummy://' . $proxyStr;
    }

    # fail if we cannot parse the url
    if (!$proxy = parse_url($proxyStr))
    {
      $params['proxyError'] = self::PROXY_ERR_INVALID;
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
      $params['proxyError'] = self::PROXY_ERR_MISSING;
      return;
    }

    # fail if scheme not one of http, https, tcp, ssl, dummy
    if (!in_array($proxy['scheme'], array('http', 'https', 'tcp', 'ssl', 'dummy')))
    {
      $params['proxyError'] = self::PROXY_ERR_UNKNOWN;
      return;
    }

    # add scheme where missing, according to port
    if ('dummy' === $proxy['scheme'])
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
        $params['proxyError'] = self::PROXY_ERR_PORT;
        return;
      }

    }

    $proxyUrl = $proxy['scheme'] . '://' . $proxy['host'] . ':' . $proxy['port'];
    $ctxUrl = str_replace(array('http://', 'https://'), array('tcp://', 'ssl://'), $proxyUrl);

    $params['http'] = array(
      'proxy' => $ctxUrl,
      'request_fulluri' => true,
    );

    if ($proxy['user'])
    {
      $auth = $proxy['user'] . ':' . $proxy['pass'];
      $params['http']['header'] = 'Proxy-Authorization: Basic '.base64_encode($auth);
      $proxyUrl = str_replace('://', '://'.$auth.'@', $proxyUrl);
    }

    $params['proxyUrl'] = $proxyUrl;

    if ('http' === $this->scheme && 0 === strpos($ctxUrl, 'ssl://'))
    {
      $params['proxyError'] = self::PROXY_ERR_SSL;
    }

  }


  /**
  * Parses http_proxy, https_proxy environment variables
  *
  * @param Array $env Environment variables
  */
  protected function proxyGetEnvironment($env)
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

    # add https value if not specified
    if ($list && !isset($list['https']))
    {
      $list['https'] = $list['http'];
    }

    return $list;

  }


  /**
  * Parses ProxyServer value from Windows registry
  *
  * Example values:
  *
  *   127.0.0.1:80
  *   - all protocols use this address
  *
  *   127.0.0.1:80;127.0.0.1:443
  *   - only http and https use a proxy
  *
  *   http=127.0.0.1:8080;https=127.0.0.1:4443
  *   - only http and https use a proxy
  *
  *   ftp=127.0.0.1:21
  *   - no proxy for http/https
  *
  * @param String $proxyStr Windows proxy value
  */
  protected function proxyGetRegistry($proxyStr)
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


  protected function proxyParams()
  {

    return array(
      'scheme' => '',
      'http' => array(),
      'proxyUrl' => '',
      'proxyDisplay' => '',
      'proxyError' => '',
    );

  }

}


class ComposerSetupWindowsRegistry
{

  /**
  * Reads a value from the registry, using Windows reg.exe
  *
  * Returns an Array of output lines or null
  *
  * @param String $key The parent key of the value
  * @param String $name The name of the value
  */
  protected function query($key, $name)
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
  * Returns True if a value is read from registry, otherwise null, putting
  * the value in $value.
  *
  * $key is in the form of ROOTKEY\SubKey name where ROOTKEY is one of:
  *   HKLM, HKCU, HKCR, HKU, HKCC
  *
  * $raw affects how the data is returned for the following values, with
  * the default format shown first:
  *
  * REG_DWORD, REG_QWORD
  *   integer, or hexadecimal if raw=true
  *
  * REG_BINARY
  *   string, or hexadecimal if raw=true
  *
  * REG_MULTI_SZ
  *   array, or string if raw=true (formatted 'sssss\0sssss\0sssss')
  *
  * @param String $key The parent key of the value
  * @param String $name The name of the value
  * @param mixed $value The returned value
  * @param Bool $raw Whether not to format certain values
  */
  public function read($key, $name, &$value, $raw = false)
  {

    $value = null;

    if (!$output = $this->query($key, $name))
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

}


class ComposerSetupRequest
{

  public $source = '';
  public $context = null;
  public $msg = '';
  public $proxyUrl = '';
  public $proxyDisplay = '';

}
