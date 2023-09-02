# Program to display the Fibonacci sequence up to n-th term

nterms = 7 # number of terms

# first two terms
n1 = 0
n2 = 1
count = 0

# check if the number of terms is valid
if nterms == 0:
   print("Please enter a number greater than zero")
# if there is only one term, return n1
elif nterms == 1:
   print("Fibonacci sequence upto 1:")
   print(n1)
# generate fibonacci sequence
else:
   print("Fibonacci sequence:")
   while count < nterms:
       print(n1)
       nth = n1 + n2
       # update values
       n1 = n2
       n2 = nth
       count = count + 1
