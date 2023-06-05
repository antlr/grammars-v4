<?php

// TODO: we need to resolve whether the grammar for PHP needs to
// support Unicode variable names. The implementation supports the
// commented out lines. The spec does not say that it does--and, in
// fact, seems to indicate the contrary:
// "
// Comment these out until ASCII vs UTF-8 with large codepoints is
// resolved.
// See discussion at https://github.com/antlr/grammars-v4/pull/2576#issuecomment-1107839866
//$привет_мир = 42;
//$𠮷 = "𠮷";

$a = 42;
$b = "𠮷";

class C{
    public $fn = 0;
}

$c = new C();
$c->fn = 1;

class CM{
    const match = 1;
}

enum EM{
    case match;
}
