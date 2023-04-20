<?php


function f($a){
    echo $a;
    return function($a){
        return f($a);
    };
}

function g(){
    f(1)(2)(3)(4)(5)(6);
}

g();
