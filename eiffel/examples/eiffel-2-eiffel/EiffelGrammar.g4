grammar EiffelGrammar;
import Eiffel;

WhiteSpace: [ \t\n\r\uFEFF]+ -> channel(1);
Comment: '--' .*? '\n' -> channel(2);
