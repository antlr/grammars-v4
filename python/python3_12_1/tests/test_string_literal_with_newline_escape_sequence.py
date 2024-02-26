# COMMAND LINE:
# grun Python file_input -tokens test_string_literal_with_newline_escape_sequence.py
#
# EXPECTATIONS:
#   - removed \<newline> escape sequence from the STRING token
#   - inserted hidden token (channel=1) with the original string literal
#   - no error message

s = 'This string will not include \
backslashes or newline characters.'
