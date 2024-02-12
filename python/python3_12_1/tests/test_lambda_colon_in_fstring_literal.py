# COMMAND LINE:
# grun Python file_input -tokens test_lambda_colon_in_fstring_literal.py
#
# EXPECTATIONS:
#   - the colon of the lambda expression is not a start of format specifier in the fstring literal
#   - no error message

print(f"{(lambda x: x*2)(3)}")
