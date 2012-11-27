<?php
namespace ProxyTests;


class Base extends \PHPUnit_Framework_TestCase
{

  public $worker = null;

  public function setUp()
  {

    $this->worker = new \RequestWorker();
    $this->initServer();

  }


  private function initServer()
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
