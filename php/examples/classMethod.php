<?php

class T{}

class C
{
    function f(): ?T
    {
        return null;
    }

}

function f(): ?T
{
    return null;
}
