~r/hello/imsx

~r|hello|

~r"hello"

~r'hello'

~r(hello)

~r[hello]

~r{hello}

~r<hello>

~w(foo bar bat)

~w(foo bar bat)a

~w(foo bar b\)at)acs

~s(this is a string with "double" quotes, not 'single' ones)

~s(String with escape codes \x26 #{"inter" <> "polation"})

~S(String without escape codes \x26 without #{interpolation})

~S"""
Converts double-quotes to single-quotes.
## Examples
    iex> convert("\"foo\"")
    "'foo'"
"""

~s/ab \u{1F600} cd/

~c(this is a char list containing "double quotes")

~D[2019-10-31]

~T[23:00:07.0]

~i(42)n