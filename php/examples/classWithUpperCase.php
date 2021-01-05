<?php

declare(strict_types=1);

namespace foobar\baz;

class MyClass extends BasicClass{
    public function __construct(int $meta = 0){
        parent::__construct(self::BASIC, $meta, "Basic");
    }
    public function getLevel() : int{
        return 123456;
    }
}
