#
cp origbcpl.g4 bcpl.g4

cat << EOF | /c/users/kenne/documents/antlrvsix/trash/bin/debug/net5-windows/trash.exe
// Read the grammar from the book.
read bcpl.g4

// Parse the grammar. This is required before any transformations can
// be done.
parse

// Unfold the "repeated_command" rule into the specific applied occurrence
// within the "repeatable_command" rule.
unfold "//parserRuleSpec[RULE_REF/text()='repeatable_command']//RULE_REF[text()='repeated_command']"

// To complete the mutual left recursion, ungroup the parentheses in the
// unfolded rule from the above transform.
ungroup "//parserRuleSpec[RULE_REF/text()='repeatable_command']//labeledAlt/alternative/element[ebnf//RULE_REF[text()='repeatable_command']]"

// Write out the file, then quit.
write
quit
EOF

# Check the grammar.
dotnet-antlr -m true -g "bcpl.g4"
cd Generated
dotnet return
dotnet build
dotnet build -t:test
cd ..
rm -rf Generated

# Fix boundary between lexer and parser.
#cp bcpl.g4 functional1-bcpl.g4

cat << EOF | /c/users/kenne/documents/antlrvsix/trash/bin/debug/net5-windows/trash.exe
read bcpl.g4
parse
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'letter']" "Letter"
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'octal_digit']" "Octal_digit"
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'hex_digit']" "Hex_digit"
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'digit']" "Digit"
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'string_constant']" "String_constant"
// must be next.
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'one_character']" "One_character"
// continue.
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'character_constant']" "Character_constant"
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'octal_number']" "Octal_number"
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'hex_number']" "Hex_number"
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'number']" "Number"
rename "//parserRuleSpec//labeledAlt//RULE_REF[text() = 'identifier']" "Identifier"
write
quit
EOF

# Check the grammar.
dotnet-antlr -m true -g "bcpl.g4"
cd Generated
dotnet return
dotnet build
dotnet build -t:test
cd ..
rm -rf Generated
