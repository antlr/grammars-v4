// Tests multi-dollar string-template interpolation.

package sample.multidollar

val oneDollar = $"one $name ${name}"
val twoDollars = $$"below $name; active $$name, $$$name, $$$$name; braced $${name}, $$$${name}"
val threeDollars = $$$"below $$name; active $$$name, $$$$name, $$$$$name; braced $$${name}, $$$$${name}"
val fourDollars = $$$$"below $$$name; active $$$$name, $$$$$name; braced $$$${name}, $$$$${name}"
val rawOne = $"""raw $name ${name}"""
val rawTwo = $$"""below $name; active $$name $${name}"""
val rawThree = $$$"""below $$name; active $$$name, $$$$name, $$$$$name; braced $$${name}, $$$$${name}
second line"""
val rawFour = $$$$"""below $$$name; active $$$$name $$$$${name}"""
val nestedLine = $$"outer $$name $${$$$"inner $$$value $$$${value}"} $$name"
val nestedRaw = $$$"""outer $$$name $$$${$$"inner $$value $${value}"} $$$name"""
val parenthesized = ($$"inside $$name")
val indexed = values[$$$"inside $$$name"][0]
val escaped = $$"escaped \$${literal}; active $$name"
