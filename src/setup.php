<?php

include(dirname(__FILE__) . DIRECTORY_SEPARATOR . 'setup.class.php');

$csStatus = 2;

$setUp = new ComposerSetup($argv, $csStatus);
$setUp->execute();

exit($csStatus);

?>
