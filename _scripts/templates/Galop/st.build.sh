# Generated from trgen <version>
set -e
if [ -f transformGrammar.py ]; then python3 transformGrammar.py; fi
GALOP=${GALOP:-galop}
for grammar in <tool_grammar_files:{g | "<g>" }; separator=" ">; do
    "$GALOP" -o . -package main "$grammar"
done
go build -o <if(os_win)>Test.exe<else>Test<endif> .
