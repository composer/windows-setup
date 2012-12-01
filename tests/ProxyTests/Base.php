<?php
namespace ProxyTests;


class Base extends \PHPUnit_Framework_TestCase
{

  public $mock = null;
  public $registry = null;

  public function setUp()
  {
    $this->mock = new \RequestBroker();
    $this->registry = $this->mock->winRegistry;
    $this->clearHttpServer();
  }


  public function setProxyEnabled($value)
  {
    $this->registry->setProxyValue('ProxyEnable', intval($value));
  }


  public function setProxyOverride($value)
  {
    $this->registry->setProxyValue('ProxyOverride', $value);
  }


  public function setProxyServer($value)
  {
    $this->registry->setProxyValue('ProxyServer', $value);
  }


  public function strContains($needle, $haystack, $case = true)
  {

    if ($case)
    {
      return false !== strpos($haystack, $needle);
    }
    else
    {
      return false !== stripos($haystack, $needle);
    }

  }



  public function clearHttpServer()
  {

    $unset = array();

    foreach ($_SERVER as $key => $value)
    {

      $lkey = strtolower($key);
      if ('http_proxy' === $lkey || 'https_proxy' === $lkey)
      {
        $unset[] = $key;
      }

    }

    foreach ($unset as $key)
    {
      unset($_SERVER[$key]);
    }

  }


}
