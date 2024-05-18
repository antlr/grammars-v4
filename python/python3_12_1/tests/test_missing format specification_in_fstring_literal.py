# COMMAND LINE:
# grun Python file_input -tokens test_missing_format_specification_in_fstring_literal.py
#
# EXPECTATIONS:
#   - inserted empty FSTRING_MIDDLE token instead of the missing format specification (after the colon)
#   - no error message

print(f"{.070:}")

