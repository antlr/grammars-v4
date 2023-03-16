<?php
enum E1
{
    case Dummy;
}

enum E{
}

enum StrEnum: string
{
    case Case1 = '1';
    case Case2 = '2';

    public function f(){}
}

enum enum : int{
    case C = 1;
}
