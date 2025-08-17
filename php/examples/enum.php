<?php
enum E1
{
    case Dummy;
}

interface I1{}

trait T1{}
trait T2{}
trait T3{}

enum E implements I1{
     case DC;
     use T1, T2;
     use T3;
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
