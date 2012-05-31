<?php
require_once '../../clients/php/Client.php';

$anekdar = new Anekdar\Client();
$anekdar->connect();
echo $anekdar->ping();
$anekdar->quit();

