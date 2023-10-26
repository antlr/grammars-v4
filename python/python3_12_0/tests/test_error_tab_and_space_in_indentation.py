# COMMAND LINE:
# grun Python file_input -tokens test_error_tab_and_space_in_indentation.py
#
# EXPECTATIONS:
#   - inserted ERROR_TOKEN instead of the WS token
#   - lexer error message: "line 11:0 inconsistent use of tabs and spaces in indentation"

if True:
    i = 0  # indented by spaces
if True:
	j = 0  # indented by a tab
