<?php

class ProxyGetParamsTest extends \ProxyTests\Base
{


  public function testDisplayHidesPassword()
  {

    $list = array(
      'http' => 'http://user:password@127.0.0.1:80',
    );

    $params = $this->mock->proxyGetParams($list, true);
    $display = $params['http']['proxyDisplay'];
    $this->assertStringStartsWith('http://user:*****@127.0.0.1:80', $display);

  }


  public function testDisplaySourceFromRegistry()
  {

    $list = array(
      'http' => 'http://127.0.0.1:80',
    );

    $params = $this->mock->proxyGetParams($list, true);
    $needle = RequestBroker::PROXY_SRC_REG;
    $display = $params['http']['proxyDisplay'];
    $this->assertTrue($this->strContains($needle, $display));

  }


  public function testDisplaySourceFromEnvironment()
  {

    $list = array(
      'http' => 'http://127.0.0.1:80',
    );

    $params = $this->mock->proxyGetParams($list, false);
    $needle = RequestBroker::PROXY_SRC_ENV;
    $display = $params['http']['proxyDisplay'];
    $this->assertTrue($this->strContains($needle, $display));

  }


  public function testErrorContainsDisplay()
  {

    $list = array(
      'http' => 'https://somewhere.com',
    );

    $this->mock->scheme = 'http';

    $params = $this->mock->proxyGetParams($list, false);
    $needle = $params['http']['proxyDisplay'];
    $error = $params['http']['proxyError'];
    $this->assertTrue($this->strContains($needle, $error));

  }


  public function testIgnoresEmpty()
  {

    $list = array(
      'http' => 'http://127.0.0.1:80',
      'https' => null,
    );

    $params = $this->mock->proxyGetParams($list, false);
    $this->assertEquals(1, count($params));

  }

}
