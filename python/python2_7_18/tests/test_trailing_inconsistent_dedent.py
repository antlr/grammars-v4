# COMMAND LINE:
# grun Python file_input -tokens test_trailing_inconsistent_dedent.py
#
# EXPECTATION:
#   - no error message

if True:
    i = 0  # the last line (next line) is an inconsistent dedent
  