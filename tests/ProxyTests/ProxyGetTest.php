<?php

class ProxyGetTest extends \ProxyTests\Base
{


  /**
  * Checks that an http_proxy var with no https_proxy var
  * is returned with http and https values
  *
  */
  public function testEnvironmentHttpReturnsBoth()
  {

    $values = array(
      'http_proxy' => 'http://127.0.0.1:8080',
    );

    $server = array_merge($_SERVER, $values);
    $list = $this->mock->proxyGetEnvironment($server);
    $this->assertArrayHasKey('http', $list);
    $this->assertArrayHasKey('https', $list);
    $this->assertEquals($values['http_proxy'], $list['https']);

  }


  /**
  * Checks that an https_proxy var with no http_proxy var
  * is returned with only an https value
  *
  */
  public function testEnvironmentHttpsReturnsHttps()
  {

    $values = array(
      'https_proxy' => 'https://127.0.0.1:4443',
    );

    $server = array_merge($_SERVER, $values);
    $list = $this->mock->proxyGetEnvironment($server);
    $this->assertArrayNotHasKey('http', $list);
    $this->assertEquals($values['https_proxy'], $list['https']);

  }


  /**
  * Checks that http_proxy and https_proxy vars
  * are returned with http and https values
  *
  */
  public function testEnvironmentBothReturnsBoth()
  {

    $values = array(
      'http_proxy' => 'http://127.0.0.1:8080',
      'https_proxy' => 'https://127.0.0.1:4443',
    );

    $server = array_merge($_SERVER, $values);
    $list = $this->mock->proxyGetEnvironment($server);
    $this->assertArrayHasKey('http', $list);
    $this->assertArrayHasKey('https', $list);
    $this->assertEquals($values['http_proxy'], $list['http']);
    $this->assertEquals($values['https_proxy'], $list['https']);

  }


  /**
  * Check that no http_proxy and no https_proxy
  * returns an empty list
  *
  */
  public function testEnvironmentNone()
  {
    $list = $this->mock->proxyGetEnvironment(null);
    $this->assertEquals(0, count($list));
  }



  /**
  * Checks that a single value
  * is returned with http and https values
  *
  */
  public function testRegistryAllReturnsBoth()
  {

    $value = '127.0.0.1:8080';
    $list = $this->mock->proxyGetRegistry($value);
    $this->assertArrayHasKey('http', $list);
    $this->assertArrayHasKey('https', $list);
    $this->assertEquals($value, $list['https']);

  }

  /**
  * Checks that a defined single http value
  * is returned with only an http value (unlike http_proxy env)
  *
  */
  public function testRegistryHttpReturnsHttp()
  {

    $value = 'http=127.0.0.1:8080';
    $list = $this->mock->proxyGetRegistry($value);
    $this->assertArrayHasKey('http', $list);
    $this->assertArrayNotHasKey('https', $list);
    $this->assertEquals('127.0.0.1:8080', $list['http']);

  }


  /**
  * Checks that a defined single https value
  * is returned with only an https value
  *
  */
  public function testRegistryHttpsReturnsHttps()
  {

    $value = 'https=127.0.0.1:4443';
    $list = $this->mock->proxyGetRegistry($value);
    $this->assertArrayHasKey('https', $list);
    $this->assertArrayNotHasKey('http', $list);
    $this->assertEquals('127.0.0.1:4443', $list['https']);

  }


  /**
  * Checks that a complex value
  * is returned with http and https values
  *
  */
  public function testRegistryBothReturnsBoth()
  {

    $value = 'https=127.0.0.1:4443;http=127.0.0.1:8080';
    $list = $this->mock->proxyGetRegistry($value);
    $this->assertArrayHasKey('http', $list);
    $this->assertArrayHasKey('https', $list);
    $this->assertEquals('127.0.0.1:8080', $list['http']);
    $this->assertEquals('127.0.0.1:4443', $list['https']);

  }


  /**
  * Checks that a malformed complex value
  * is returned with http and https values
  *
  */
  public function testRegistryMalformedBothReturnsBoth()
  {

    $value = '=https= 127.0.0.1:4443; http =127.0.0.1:8080';
    $list = $this->mock->proxyGetRegistry($value);
    $this->assertArrayHasKey('http', $list);
    $this->assertArrayHasKey('https', $list);
    $this->assertEquals('127.0.0.1:8080', $list['http']);
    $this->assertEquals('127.0.0.1:4443', $list['https']);

  }


  /**
  * Checks that a complex value other than http, https
  * is returned empty
  *
  */
  public function testRegistryOtherReturnsNone()
  {

    $value = 'ftp=127.0.0.1:4443;socks=127.0.0.1:5000';
    $list = $this->mock->proxyGetRegistry($value);
    $this->assertEquals(0, count($list));

  }


  /**
  * Checks that a complex value with https
  * returns only an http value
  *
  */
  public function testRegistryHttpOtherReturnsHttp()
  {

    $value = 'ftp=127.0.0.1:4443;http=127.0.0.1:8080;socks=127.0.0.1:5000';
    $list = $this->mock->proxyGetRegistry($value);
    $this->assertEquals(1, count($list));
    $this->assertEquals('127.0.0.1:8080', $list['http']);

  }


  /**
  * Check that no reg value
  * returns an empty list
  *
  */
  public function testRegistryNoneReturnsNone()
  {
    $list = $this->mock->proxyGetRegistry(null);
    $this->assertEquals(0, count($list));
  }



}