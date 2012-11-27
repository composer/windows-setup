<?php
namespace RegistryTests;


class Base extends \PHPUnit_Framework_TestCase
{

  public $worker = null;

  public function setUp()
  {
    $this->worker = new RegistryWorker();
  }

}


class RegistryWorker extends \RequestWorker
{

  private $value = '';
  private $type = 0;

  const REG_DWORD = 1;
  const REG_SZ = 2;
  const REG_MULTI_SZ = 3;
  const REG_BINARY = 4;


  public function winRegTestGet(&$value, $raw = false)
  {
    return $this->winRegGet('HKCU\Test\Path', 'TestKey', $value, $raw);
  }

  public function winRegRead($key, $name)
  {

    $output = array();
    $tab = str_repeat(' ', 4);

    if ($type = $this->getRegType())
    {
      $output[] = 'Key name';
      $output[] = $tab.$name.$tab.$type.$tab.$this->value;
      return $output;
    }

    $output[] = '';
    $output[] = 'ERROR: The system was unable to find the specified registry key or value.';
    return $output;

  }

  public function setValue($type, $value)
  {
    $this->type = $type;
    $this->value = $value;
  }

  private function getRegType()
  {

    switch ($this->type)
    {
      case self::REG_DWORD: return 'REG_DWORD';
      case self::REG_SZ: return 'REG_SZ';
      case self::REG_MULTI_SZ: return 'REG_MULTI_SZ';
      case self::REG_BINARY: return 'REG_BINARY';
      default: return '';
    }

  }

}
