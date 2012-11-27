<?php


class RequestWorker extends ComposerSetupRequest
{

  public function __call($method, $params)
  {
    return call_user_func_array(array($this, $method), $params);
  }

  public function __get($property)
  {
    return $this->$property;
  }

  public function __set($property, $value)
  {
    $this->$property = $value;
  }

}
?>
