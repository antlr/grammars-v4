def inc(value):
# this is a comment (or type comment)
    return value + 1

# COMMAND LINE:
# grun Python file_input -tokens test_hidden_NEWLINE_before_comment.py
#
# EXPECTATIONS:
#   - hidden NEWLINE tokens (channel=1) before a COMMENT (or a TYPE_COMMENT) token
#   - hidden NEWLINE token (channel=1) before the blank line
#   - no error message
