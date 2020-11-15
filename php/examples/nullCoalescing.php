<?php
$username = $_GET['user'] ?? 'nobody';
$username = $_GET['user'] ?? $_POST['user'] ?? 'nobody';
$array['key'] ??= computeDefault();
?>