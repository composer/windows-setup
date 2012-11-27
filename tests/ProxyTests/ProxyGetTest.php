<?php

class ProxyGetTest extends \ProxyTests\Base
{


  /**
  * Checks that an http_proxy var with no https_proxy var
  * is returned with http and https values
  *
  */
  public function testGetEnvironmentHttp()
  {

    $values = array(
      'http_proxy' => '127.0.0.1:8080',
    );

    $server = array_merge($_SERVER, $values);
    $list = $this->worker->envGetProxies($server);
    $this->assertArrayHasKey('http', $list);
    $this->assertArrayHasKey('https', $list);
    $this->assertEquals($values['http_proxy'], $list['https']);

  }


  /**
  * Checks that an https_proxy var with no http_proxy var
  * is returned with only an https value
  *
  */
  public function testGetEnvironmentHttps()
  {

    $values = array(
      'https_proxy' => '127.0.0.1:8080',
    );

    $server = array_merge($_SERVER, $values);
    $list = $this->worker->envGetProxies($server);
    $this->assertArrayNotHasKey('http', $list);
    $this->assertEquals($values['https_proxy'], $list['https']);

  }


  /**
  * Check that no http_proxy and no https_proxy
  * returns an empty list
  *
  */
  public function testGetEnvironmentNone()
  {
    $list = $this->worker->envGetProxies(null);
    $this->assertEquals(0, count($list));
  }



  /**
  * Checks that a single value
  * is returned with http and https values
  *
  */
  public function testGetRegSimple()
  {

    $value = '127.0.0.1:8080';
    $list = $this->worker->regGetProxies($value);
    $this->assertArrayHasKey('http', $list);
    $this->assertArrayHasKey('https', $list);
    $this->assertEquals($value, $list['https']);

  }

  /**
  * Checks that a defined single http value
  * is returned with only an http value (unlike http_proxy env)
  *
  */
  public function testGetRegDefinedHttp()
  {

    $value = 'http=127.0.0.1:8080';
    $list = $this->worker->regGetProxies($value);
    $this->assertArrayHasKey('http', $list);
    $this->assertArrayNotHasKey('https', $list);
    $this->assertEquals('127.0.0.1:8080', $list['http']);

  }


  /**
  * Checks that a defined single https value
  * is returned with only an https value
  *
  */
  public function testGetRegDefinedHttps()
  {

    $value = 'https=127.0.0.1:4443';
    $list = $this->worker->regGetProxies($value);
    $this->assertArrayHasKey('https', $list);
    $this->assertArrayNotHasKey('http', $list);
    $this->assertEquals('127.0.0.1:4443', $list['https']);

  }


  /**
  * Checks that a complex value
  * is returned with http and https values
  *
  */
  public function testGetRegDefined()
  {

    $value = 'https=127.0.0.1:4443;http=127.0.0.1:8080';
    $list = $this->worker->regGetProxies($value);
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
  public function testGetMalformedRegDefined()
  {

    $value = '=https= 127.0.0.1:4443; http =127.0.0.1:8080';
    $list = $this->worker->regGetProxies($value);
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
  public function testGetRegDefinedNone()
  {

    $value = 'ftp=127.0.0.1:4443;socks=127.0.0.1:5000';
    $list = $this->worker->regGetProxies($value);
    $this->assertEquals(0, count($list));

  }


  /**
  * Checks that a complex value with https
  * returns only an http value
  *
  */
  public function testGetRegDefinedMixed()
  {

    $value = 'ftp=127.0.0.1:4443;http=127.0.0.1:8080;socks=127.0.0.1:5000';
    $list = $this->worker->regGetProxies($value);
    $this->assertEquals(1, count($list));
    $this->assertEquals('127.0.0.1:8080', $list['http']);

  }


  /**
  * Check that no reg value
  * returns an empty list
  *
  */
  public function testGetRegNone()
  {
    $list = $this->worker->regGetProxies(null);
    $this->assertEquals(0, count($list));
  }



}