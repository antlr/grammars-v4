# COMMAND LINE:
# grun Python file_input -tokens test_explicit_line_joining.py
#
# EXPECTATIONS:
#   - hiden (channel=1) LINE_JOINING token
#   - no error message

i = 1 \
  + 2
