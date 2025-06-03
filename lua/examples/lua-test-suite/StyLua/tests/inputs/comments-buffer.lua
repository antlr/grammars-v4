local foo_result = foo( --a comment
	"oof"
)

local expr_result = 1 + 2 + 3 + 4 + 5 --a comment
	+ 6 + 6 + 8

print"text" --a comment
foo{bar = baz} -- comment

for foo, -- test
bar in 
next, -- comment
value
do
	print("test", -- comment
		"foo"
	)
end

if code == 9 -- \t
or code == 32 -- <space>
   then
    print(code)
end

return foo, -- a comment
bar