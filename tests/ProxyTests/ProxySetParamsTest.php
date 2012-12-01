<?php

class ProxySetParamsTest extends \ProxyTests\Base
{

  public function testErrorUnc()
  {

    $proxyStr = '\\\\server';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);
    $needle = RequestBroker::PROXY_ERR_UNC;
    $this->assertTrue($this->strContains($needle, $params['proxyError']));

  }


  public function testErrorInvalid()
  {

    $proxyStr = '//';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);
    $needle = RequestBroker::PROXY_ERR_INVALID;
    $this->assertTrue($this->strContains($needle, $params['proxyError']));

  }


  public function testErrorMissing()
  {

    $proxyStr = '://';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);
    $needle = RequestBroker::PROXY_ERR_MISSING;
    $this->assertTrue($this->strContains($needle, $params['proxyError']));
  }


  public function testErrorUnknown()
  {

    $proxyStr = 'fred://somewhere.com';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);
    $needle = RequestBroker::PROXY_ERR_UNKNOWN;
    $this->assertTrue($this->strContains($needle, $params['proxyError']));

  }


  public function testErrorPort()
  {

    $proxyStr = 'tcp://somewhere.com';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);
    $needle = RequestBroker::PROXY_ERR_PORT;
    $this->assertTrue($this->strContains($needle, $params['proxyError']));

  }


  public function testErrorOpenSsl()
  {

    $proxyStr = 'https://somewhere.com';
    $this->mock->scheme = 'http';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);
    $needle = RequestBroker::PROXY_ERR_SSL;
    $this->assertTrue($this->strContains($needle, $params['proxyError']));

  }


  public function testMissingSchemeHttpPort80()
  {

    $proxyStr = 'somewhere.com:80';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);
    $this->assertEquals('http://somewhere.com:80', $params['proxyUrl']);

  }


  public function testMissingSchemeHttpsPort443()
  {

    $proxyStr = 'somewhere.com:443';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);
    $this->assertEquals('https://somewhere.com:443', $params['proxyUrl']);

  }


  public function testMissingSchemeHttpNoPort()
  {

    $proxyStr = 'somewhere.com';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);
    $this->assertEquals('http://somewhere.com:80', $params['proxyUrl']);

  }


  public function testProxyUrlHttp()
  {

    $proxyStr = 'http://user:password@somewhere.com';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);

    $this->assertEquals('http://user:password@somewhere.com:80', $params['proxyUrl']);
    $this->assertEquals('tcp://somewhere.com:80', $params['http']['proxy']);
    $this->assertEquals(true, $params['http']['request_fulluri']);

    $expects = 'Proxy-Authorization: Basic '. base64_encode('user:password');
    $this->assertEquals($expects, $params['http']['header']);

  }


  public function testProxyUrlHttps()
  {

    $proxyStr = 'https://user:password@somewhere.com';
    $params = null;

    $this->mock->proxySetParams($proxyStr, $params);

    $this->assertEquals('https://user:password@somewhere.com:443', $params['proxyUrl']);
    $this->assertEquals('ssl://somewhere.com:443', $params['http']['proxy']);
    $this->assertEquals(true, $params['http']['request_fulluri']);

    $expects = 'Proxy-Authorization: Basic '. base64_encode('user:password');
    $this->assertEquals($expects, $params['http']['header']);

  }

}