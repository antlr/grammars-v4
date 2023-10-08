# COMMAND LINE:
# grun Python file_input -tokens test_error_unexpected_indent.py
#
# REQUIREMENT:
#   - parser error message: "line 9:7 extraneous input '<INDENT>' ..."

if True:
    i = 0
       j = 1  # invalid indentation
