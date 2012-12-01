<?php


class RequestBroker extends ComposerSetupRequestBroker
{

  public function __construct()
  {
    parent::__construct();
    $this->winRegistry = new MockRegistry();
  }


  public function proxySetParams($proxyStr, &$params)
  {
    return parent::proxySetParams($proxyStr, $params);
  }

  public function __call($method, $params)
  {
    return call_user_func_array(array($this, $method), $params);
  }

  public function __get($property)
  {
    return $this->$property;
  }

  public function __set($property, $value)
  {
    $this->$property = $value;
  }

}


class MockRegistry extends ComposerSetupWindowsRegistry
{

  private $reg = array();
  private $types = array();

  const KEY_PROXY = 'HKCU\Software\Microsoft\Windows\CurrentVersion\Internet Settings';
  const KEY_TYPE = 'HKCR\RegistryTypes';


  public function __construct()
  {
    $this->initReg();
  }


  public function query($key, $name)
  {

    $output = array();
    $tab = str_repeat(' ', 4);

    if ($res = $this->queryWork($key, $name))
    {
      list($key, $type, $value) = $res;
      $output[] = $key;
      $output[] = $tab.$name.$tab.$type.$tab.$value;
    }
    else
    {
      $output[] = '';
      $output[] = 'ERROR: The system was unable to find the specified registry key or value.';
    }

    return $output;

  }


  public function setValue($key, $type, $name, $value)
  {

    if (!$this->checkValue($type, $value))
    {
      throw new \Exception('Invalid value');
    }

    if (!isset($this->reg[$key]))
    {
      $this->reg[$key] = array();
    }

    if (!isset($this->reg[$key][$name]))
    {
      $this->reg[$key][$name] = array();
    }

    $this->reg[$key][$name] = array('type' => $type, 'value' => $value);

  }


  public function setProxyValue($name, $value)
  {

    $key = self::KEY_PROXY;

    if ('ProxyEnable' === $name)
    {
      $this->setValue($key, 'REG_DWORD', $name, $value);
    }
    elseif ('ProxyOverride' === $name || 'ProxyServer' === $name)
    {
      $this->setValue($key, 'REG_SZ', $name, $value);
    }

  }


  public function setTypeValue($type, $value)
  {
    $this->setValue(self::KEY_TYPE, $type, $type, $value);
  }


  public function unsetKey($key)
  {

    if (isset($this->reg[$key]))
    {
      unset($this->reg[$key]);
    }

  }


  public function unsetValueName($key, $name)
  {

    if (isset($this->reg[$key]) && isset($this->reg[$key][$name]))
    {
      unset($this->reg[$key][$name]);
    }

  }


  private function checkValue($type, &$value)
  {

    if (!in_array($type, $this->types))
    {
      return;
    }

    if ('REG_DWORD' === $type || 'REG_QWORD' === $type)
    {

      if (is_int($value))
      {
        $value = '0x'.dechex($value);
        return true;
      }

    }
    elseif ('REG_MULTI_SZ' === $type)
    {

      if (is_array($value))
      {
        $value = implode('\0', $value);
        return true;
      }

    }
    else
    {

      if (is_string($value))
      {

        if ('REG_BINARY' === $type)
        {
          $value = bin2hex($value);
        }

      }

      return true;

    }

  }


  private function initReg()
  {

    $this->types = array(
      'REG_DWORD',
      'REG_QWORD',
      'REG_SZ',
      'REG_MULTI_SZ',
      'REG_BINARY',
      );

    $this->setProxyValue('ProxyEnable', 0);
    $this->setProxyValue('ProxyOverride', '');
    $this->setProxyValue('ProxyServer', '');

  }


  private function queryWork($key, $name)
  {

    if (!isset($this->reg[$key]) || !isset($this->reg[$key][$name]))
    {
      return;
    }

    if (!$pos = strpos($key, '\\'))
    {
      return;
    }

    $rootKey = substr($key, 0, $pos);
    $subKey = substr($key, $pos);

    switch ($rootKey)
    {

      case 'HKLM': $root = 'HKEY_LOCAL_MACHINE';
        break;

      case 'HKCU': $root = 'HKEY_CURRENT_USER';
        break;

      case 'HKCR': $root = 'HKEY_CLASSES_ROOT';
        break;

      case 'HKU': $root = 'HKEY_USERS';
        break;

      case 'HKCC': $root = 'HKEY_CURRENT_CONFIG';
        break;

      default: return;

    }

    $fullKey = $root.$subKey;
    $type = $this->reg[$key][$name]['type'];
    $value = $this->reg[$key][$name]['value'];

    return array($fullKey, $type, $value);

  }


}

