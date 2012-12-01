<?php

class RegistryTest extends \RegistryTests\Base
{


  public function testDwordReturnsInteger()
  {

    $name = 'REG_DWORD';
    $value = 16;
    $this->mock->setTypeValue($name, $value);

    $expected = $value;
    $this->mock->read($this->key, $name, $actual);
    $this->assertTrue($expected === $actual);

  }


  public function testDwordReturnsStringHex()
  {

    $name = 'REG_DWORD';
    $value = 32;
    $this->mock->setTypeValue($name, $value);

    $expected = '0x20';
    $this->mock->read($this->key, $name, $actual, true);
    $this->assertTrue($expected === $actual);

  }


  public function testSzReturnsStringValue()
  {

    $name = 'REG_SZ';
    $value = 'string from registry';
    $this->mock->setTypeValue($name, $value);

    $expected = $value;
    $this->mock->read($this->key, $name, $actual);
    $this->assertEquals($expected, $actual);

  }


  public function testSzReturnsStringEmpty()
  {

    $name = 'REG_SZ';
    $value = '';
    $this->mock->setTypeValue($name, $value);
    $expected = $value;

    $this->mock->read($this->key, $name, $actual);
    $this->assertEquals($expected, $actual);

  }


  public function testMultiSzReturnsArray()
  {

    $name = 'REG_MULTI_SZ';
    $value = array('value1 ', 'value 2', ' value3');
    $this->mock->setTypeValue($name, $value);

    $expected = $value;
    $this->mock->read($this->key, $name, $actual);
    $this->assertTrue($expected === $actual);

  }


  public function testReadMultiSzReturnsStringFormatted()
  {

    $name = 'REG_MULTI_SZ';
    $value = array('value1', 'value 2', 'value3');
    $this->mock->setTypeValue($name, $value);

    $expected = 'value1\0value 2\0value3';
    $this->mock->read($this->key, $name, $actual, true);
    $this->assertEquals($expected, $actual);

  }


  public function testBinaryReturnsString()
  {

    $name = 'REG_BINARY';
    $value = "reg\r\n";
    $this->mock->setTypeValue($name, $value);

    $expected = $value;
    $this->mock->read($this->key, $name, $actual);
    $this->assertEquals($expected, $actual);

  }


  public function testBinaryReturnsStringHex()
  {

    $name = 'REG_BINARY';
    $value = "reg\r\n";
    $this->mock->setTypeValue($name, $value);

    $expected = '7265670d0a';
    $this->mock->read($this->key, $name, $actual, true);
    $this->assertEquals($expected, $actual);

  }

  public function testNonExistent()
  {

    $name = 'REG_SZ';
    $expected = null;
    $this->mock->read($this->key, $name, $actual);
    $this->assertEquals($expected, $actual);

  }


}