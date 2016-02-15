<?php

// require like this for cygwin php
require('./setup.class.php');

$csStatus = 2;

$setUp = new ComposerSetup($argv, $csStatus);
$setUp->execute();

exit($csStatus);

?>
