<?php

class T{}

class C
{
    function __construct (protected readonly T $p)
    {

    }

    function readonly($readonly){

    }
}

function readonly($readonly){

}
