// enum and enum_flags declarations, and a switch over an enum value.

enum Color:
    red
    green
    blue

enum_flags Flags:
    flag_a = 1
    flag_b
    flag_c

def name_of(c:Color) -> string:
    switch c:
        case red: return "red"
        case green: return "green"
        case blue: return "blue"

def main():
    let c = green
    print "color = {name_of(c)}"

main()
