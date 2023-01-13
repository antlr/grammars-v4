<?php

foo(<<< HEREDOC
Heredoc line 1.
Heredoc line 2.
HEREDOC
);

foo(<<< 'NOWDOC'
Nowdoc line 1.
Nowdoc line 2.
NOWDOC
);

$str = "asdf";

$str1 = <<<HEREDOC1
Hello world!
HEREDOC1;

?>