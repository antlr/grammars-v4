// https://www.php.net/manual/en/migration70.new-features.php#119625
// php7 only
<?php
class foo { static $bar = 'baz'; }
var_dump('foo'::$bar);