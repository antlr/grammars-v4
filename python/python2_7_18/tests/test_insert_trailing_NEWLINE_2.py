# COMMAND LINE:
# grun Python file_input -tokens test_insert_trailing_NEWLINE_2.py
#
# EXPECTATIONS:
#   - inserted trailing NEWLINE token
#   - inserted trailing DEDENT token
#   - no error message

if True:
    j = 0   # there is no newline at the end of this code