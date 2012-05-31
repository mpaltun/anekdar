<?php
require_once 'Client.php';

$anekdar = new Anekdar\Client();
$anekdar->connect();
echo $anekdar->ping();
$anekdar->quit();

