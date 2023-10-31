# COMMAND LINE:
# grun Python file_input -tokens test_formfeed_in_indent.py
#
# EXPECTATION: no error message

if True:
     i = 1 # the indentation length starts after the last formfeed
  j = 1
