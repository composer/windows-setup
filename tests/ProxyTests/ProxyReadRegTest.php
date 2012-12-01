<?php

class ProxyReadRegTest extends \ProxyTests\Base
{


  /**
  * Checks that null is returned when ProxyEnabled is false
  *
  */
  public function testProxyEnabledReturnsNull()
  {

    $this->setProxyEnabled(false);
    $this->setProxyServer('http=127.0.0.1:8080');
    $actual = $this->mock->proxyReadRegistry('');
    $this->assertNull($actual);

  }


  /**
  * Checks that null is returned when ProxyOverride
  * matches url
  *
  */
  public function testProxyOverrideMatchReturnsNull()
  {

    $this->setProxyEnabled(true);
    $this->setProxyOverride('somewhere.com');
    $url = 'http://somewhere.com/something';
    $actual = $this->mock->proxyReadRegistry($url);
    $this->assertNull($actual);

  }


  /**
  * Checks that null is not returned when ProxyOverride
  * does not match url
  *
  */
  public function testProxyOverrideNotMatchReturnsProxyServer()
  {

    $this->setProxyEnabled(true);
    $this->setProxyOverride('somewhere.else.com');
    $this->setProxyServer('http=127.0.0.1:8080');

    $url = 'http://somewhere.com/something';
    $actual = $this->mock->proxyReadRegistry($url);
    $this->assertEquals('http=127.0.0.1:8080', $actual);


  }


  /**
  * Checks that ProxyServer is returned when
  * ProxyEnabled is true and no ProxyOverride
  *
  */
  public function testProxyServerReturnsProxyServer()
  {

    $this->setProxyEnabled(true);
    $this->setProxyServer('http=127.0.0.1:8080');

    $url = 'http://somewhere.com/something';
    $actual = $this->mock->proxyReadRegistry('');
    $this->assertEquals('http=127.0.0.1:8080', $actual);

  }



}
