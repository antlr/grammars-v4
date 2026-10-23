<?php

class C{
    const G = 'a'."b";
    const CC = D::F.self::G."b".'s';
    const DD = "a".'b'.'c'."d"."e".G.G.G.'f'.self::G;
}
