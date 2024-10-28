# COMMAND LINE:
# grun Python file_input -tokens test_match_case.py
#
# EXPECTATIONS:
#   - [@63,234:234='*',<'*'>,12:13]
#   - [@64,235:235='_',<NAME>,12:14]
#   - no error message

a, *b = [1, 2, 3, 4]
match b:
    case [2]:
        print("0")
    case [f, *_] if f==2:
        print("1")
    case _:
        print("2")
