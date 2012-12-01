<?php

class ProxyProcessTest extends \ProxyTests\Base
{


  public function testSchemeHttpProxyHttpsReturnsNull()
  {

    $list = array(
      'http' => 'http://127.0.0.1:8080',
    );

    $this->mock->scheme = 'https';

    $proxyList = $this->mock->proxyProcess($list, true);
    $this->assertNull($proxyList);

  }


  public function testSchemeHttpsProxyHttpReturnsNull()
  {

    $list = array(
      'https' => 'http://127.0.0.1:4443',
    );

    $this->mock->scheme = 'http';

    $proxyList = $this->mock->proxyProcess($list, true);
    $this->assertNull($proxyList);

  }


  public function testSchemeHttpProxyHttpReturnsHttp()
  {

    $list = array(
      'http' => 'http://127.0.0.1:8080',
    );

    $this->mock->scheme = 'http';

    $proxyList = $this->mock->proxyProcess($list, true);

    $this->assertEquals(1, count($proxyList));
    $params = $proxyList[0];
    $this->assertEquals('http', $params['scheme']);
    $this->assertEquals('http://127.0.0.1:8080', $params['proxyUrl']);

  }


  public function testSchemeHttpsProxyHttpsReturnsHttps()
  {

    $list = array(
      'https' => 'http://127.0.0.1:4443',
    );

    $this->mock->scheme = 'https';

    $proxyList = $this->mock->proxyProcess($list, true);

    $this->assertEquals(1, count($proxyList));
    $params = $proxyList[0];
    $this->assertEquals('https', $params['scheme']);
    $this->assertEquals('http://127.0.0.1:4443', $params['proxyUrl']);

  }


  public function testSchemeHttpProxyBothSameReturnHttp()
  {

    $list = array(
      'http' => 'http://127.0.0.1:5454',
      'https' => 'http://127.0.0.1:5454',
    );

    $this->mock->scheme = 'http';

    $proxyList = $this->mock->proxyProcess($list, true);

    $this->assertEquals(1, count($proxyList));
    $params = $proxyList[0];
    $this->assertEquals('http', $params['scheme']);
    $this->assertEquals('http://127.0.0.1:5454', $params['proxyUrl']);

  }


  public function testSchemeHttpsProxyBothSameReturnHttps()
  {

    $list = array(
      'http' => 'http://127.0.0.1:5454',
      'https' => 'http://127.0.0.1:5454',
    );

    $this->mock->scheme = 'https';

    $proxyList = $this->mock->proxyProcess($list, true);

    $this->assertEquals(1, count($proxyList));
    $params = $proxyList[0];
    $this->assertEquals('https', $params['scheme']);
    $this->assertEquals('http://127.0.0.1:5454', $params['proxyUrl']);

  }


  public function testSchemeHttpProxyBothDifferentReturnHttp()
  {

    $list = array(
      'http' => 'http://127.0.0.1:8080',
      'https' => 'http://127.0.0.1:4443',
    );

    $this->mock->scheme = 'http';

    $proxyList = $this->mock->proxyProcess($list, true);

    $this->assertEquals(1, count($proxyList));
    $params = $proxyList[0];
    $this->assertEquals('http', $params['scheme']);
    $this->assertEquals('http://127.0.0.1:8080', $params['proxyUrl']);

  }


  public function testSchemeHttpsProxyBothDifferentReturnBoth()
  {

    $list = array(
      'http' => 'http://127.0.0.1:8080',
      'https' => 'http://127.0.0.1:4443',
    );

    $this->mock->scheme = 'https';

    $proxyList = $this->mock->proxyProcess($list, true);

    $this->assertEquals(2, count($proxyList));
    $params = $proxyList[0];
    $this->assertEquals('https', $params['scheme']);
    $this->assertEquals('http://127.0.0.1:4443', $params['proxyUrl']);

    $params = $proxyList[1];
    $this->assertEquals('http', $params['scheme']);
    $this->assertEquals('http://127.0.0.1:8080', $params['proxyUrl']);

  }



}
