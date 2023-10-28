# COMMAND LINE:
# grun Python file_input -tokens test_error_first_statement_indented.py
#
# EXPECTATIONS:
#   - inserted leading INDENT token
#   - hidden NEWLINE tokens (channel=1) before the first statement
#   - lexer error message: "line 10:3 first statement indented"


   i = 1   # first statement begins with space
