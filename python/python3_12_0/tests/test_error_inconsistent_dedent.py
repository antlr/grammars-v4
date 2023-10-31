# COMMAND LINE:
# grun Python file_input -tokens test_error_inconsistent_dedent.py
#
# EXPECTATIONS:
#   - inserted ERROR_TOKEN instead of the DEDENT token
#   - lexer error message: "line 10:0 inconsistent dedent"

if True:
    i = 0
  j = 0  # inconsistent dedent
