# COMMAND LINE:
# grun Python file_input -tokens test_double_braces_in_fstring_literal.py
#
# EXPECTATIONS:
#   - replace the double braces '{{' or '}}' to single braces: '{' or '}'
#   - inserted hidden second brace token (channel=1)
#   - no error message

print(f"{{ {4*10} }}")
