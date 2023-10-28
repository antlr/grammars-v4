# COMMAND LINE:
# grun Python file_input -tokens test_implicit_line_joining.py
#
# EXPECTATIONS:
#   - hidden NEWLINE token (channel=1) after the opening parenthesis
#   - no error message

print(1
    + 2)
