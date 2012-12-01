<?php
namespace RegistryTests;

class Base extends \PHPUnit_Framework_TestCase
{

  /** @var \MockRegistry */
  public $mock = null;
  public $key;

  public function setUp()
  {
    $this->mock = new \MockRegistry();
    $this->key = \MockRegistry::KEY_TYPE;
  }

}
