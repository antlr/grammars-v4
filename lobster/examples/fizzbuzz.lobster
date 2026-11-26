// Classic fizzbuzz: if / elif / else chains with inline bodies.

def fizzbuzz(n:int):
    for(n) i:
        let x = i + 1
        if x % 15 == 0: print "FizzBuzz"
        elif x % 3 == 0: print "Fizz"
        elif x % 5 == 0: print "Buzz"
        else: print "{x}"

def main():
    fizzbuzz(20)

main()
