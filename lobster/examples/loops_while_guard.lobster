// while loops with compound assignment, and guard statements that bail
// out of a function early with cleanup code.

def countdown(n:int):
    var i = n
    while i > 0:
        print "{i}"
        i -= 1
    print "liftoff!"

def safe_divide(a:float, b:float) -> float:
    guard b != 0.0:
        print "error: division by zero"
        return 0.0
    return a / b

def main():
    countdown(5)
    print "{safe_divide(10.0, 2.0)}"
    print "{safe_divide(10.0, 0.0)}"

main()
