<?php

use \RegistryTests\RegistryWorker;

class RegistryTest extends \RegistryTests\Base
{


  public function testReadDword()
  {

    $value = '0x20';
    $this->worker->setValue(RegistryWorker::REG_DWORD, $value);
    $expected = intval($value, 16);

    $this->worker->winRegTestGet($actual);
    $this->assertTrue($expected === $actual);

  }


  public function testReadDwordRaw()
  {

    $value = '0x20';
    $this->worker->setValue(RegistryWorker::REG_DWORD, $value);
    $expected = $value;

    $this->worker->winRegTestGet($actual, true);
    $this->assertTrue($expected === $actual);

  }

  public function testFailReadDwordRaw()
  {

    $value = '0x20';
    $this->worker->setValue(RegistryWorker::REG_DWORD, $value);
    $expected = $value;

    $this->worker->winRegTestGet($actual);
    $this->assertTrue($expected !== $actual);

  }


  public function testReadSz()
  {

    $value = 'string from registry';
    $this->worker->setValue(RegistryWorker::REG_SZ, $value);
    $expected = $value;

    $this->worker->winRegTestGet($actual);
    $this->assertEquals($expected, $actual);

  }


  public function testReadSzEmpty()
  {

    $value = '';
    $this->worker->setValue(RegistryWorker::REG_SZ, $value);
    $expected = $value;

    $this->worker->winRegTestGet($actual);
    $this->assertEquals($expected, $actual);

  }


  public function testReadMultiSz()
  {

    $value = 'value1\0value2\0value3';
    $this->worker->setValue(RegistryWorker::REG_MULTI_SZ, $value);
    $expected = array('value1', 'value2', 'value3');

    $this->worker->winRegTestGet($actual);
    $this->assertTrue($expected === $actual);

  }


  public function testReadMultiSzRaw()
  {

    $value = 'value1\0value2\0value3';
    $this->worker->setValue(RegistryWorker::REG_MULTI_SZ, $value);
    $expected = $value;

    $this->worker->winRegTestGet($actual, true);
    $this->assertEquals($expected, $actual);

  }


  public function testReadBinary()
  {

    $value = '7265670d0a';
    $this->worker->setValue(RegistryWorker::REG_BINARY, $value);
    $expected = "reg\r\n";

    $this->worker->winRegTestGet($actual);
    $this->assertEquals($expected, $actual);

  }


  public function testReadBinaryRaw()
  {

    $value = '7265670d0a';
    $this->worker->setValue(RegistryWorker::REG_BINARY, $value);
    $expected = $value;

    $this->worker->winRegTestGet($actual, true);
    $this->assertEquals($expected, $actual);

  }

  public function testReadNonExistent()
  {

    $value = null;
    $this->worker->setValue(0, $value);
    $expected = $value;

    $this->worker->winRegTestGet($actual, true);
    $this->assertEquals($expected, $actual);

  }


}