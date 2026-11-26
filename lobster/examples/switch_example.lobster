// switch / case / default, including an inclusive numeric range case.
// Note the spaces around ".." below: without them "3..9" would lex as
// two float literals ("3." and ".9") rather than "3" RANGE "9".

def describe(n:int):
    switch n:
        case 0: print "zero"
        case 1, 2: print "small"
        case 3 .. 9: print "medium"
        default: print "large"

def main():
    for(12) i:
        describe(i)

main()
