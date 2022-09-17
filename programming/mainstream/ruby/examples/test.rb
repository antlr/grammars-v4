a = 1.1+2.5*2
b = 1
c = 100+(100*3)
a = 1

# This is comment

dyn = a+4+2
dyn += 5.2 + c * (b + 1)

d = "abc"
d = "abc"+"def"
d = "abc"*3
d = 'abc'
d = 'abc'+'def'
d = 'abc'*3

mas = []
mas[1] = 5
mas[2] = "String"

a = mas[2]

a = 2

if a == 1
  a = 2
  unless b == 2
    b = 3
  end
else
  a = 3
end

for(i = 0, a = 0; i<10 && a<20; i+=1, a+=1)
  a += 1
  if a == 1
    b += 2*5
  end
end

a = 10
while a > 0
  a -= 1
end

pir
  $P0 = "333444"
  $I0 = 19
end

i=10
for i = 0; i<10; i+=1
  if i==3
    break
  end
  a = 10
  while a > 0
    puts "hahaha"
    if a==2
      break
    end
    a -= 1
  end
end

func0()
func1(1)
func2(2+3, 3, "Hello "+"world!")
func3(a+t,b,c)
func4(a=4+3, b="Hello "+"world!")

def func1 (a,b,c)
  a = 1
  return a
end

def func2 ()
  a = []
  a[1] = 2
  a[2] = 3
  b = a[2]
  return b
end

a = 4
exp1 = 100+2*3/(3+4*3)
exp2 = 100.5+2*3.1/(3+4*3)
exp3 = "hello "*2
exp4 = a + 2*3

a = func(1)*func2(5)

if "a" < "b"
  puts "a < b"
else
  puts "a > b"
end

def func(a)
  return a*2
end

def func2(a)
  return 1
end

puts "Hello, I'm CORUN\"DUM"
puts 'hahaha!'
puts a

if 'a' > 'b'
  puts "a > b"
elsif 'a' < 'b'
  puts "a < b"
end

if 'a' > 'b'
  puts "a > b"
elsif 'c' > 'd'
  puts "c > d"
elsif 'c' < 'd'
  puts "c < d"
end

unless 'a' < 'b'
  puts "a > b"
  elsif 'c' > 'd'
  puts "c > d"
else
  puts "c < d"
end

mas = []
mas = read_file("lab4.txt")

length = len(mas)

for(i = 1; i < length - 1; i+=1)
  for(j = 0; j < length - i; j+=1)
    if(mas[j] > mas[j+1])
      buf = mas[j]
      mas[j] = mas[j+1]
      mas[j+1] = buf
    end
  end
end

for(i = 0; i < length; i+=1)
  puts mas[i]
end


s = "hello"
l = len(s)
puts l

a = func(1,2)
puts a

def func a, b
  return a+b
end
