# COMMAND LINE:
# grun Python file_input -tokens test_error_not_indented.py
#
# EXPECTATION:
#   - parser error message: "line 8:0 missing INDENT at 'i'"

if True:
i = 1  # no indentation
