# Generated from trgen <version>
rm -f *.tokens *.interp
rm -f <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
rm -f ./<if(os_win)>Test.exe<else>Test<endif>
