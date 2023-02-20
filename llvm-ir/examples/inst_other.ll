define i32 @g() {
0:
	ret i32 42
}

define void @h(i32 %x) {
0:
	ret void
}

define void @f() {
0:
	%1 = icmp eq i32 1, 2
	br i1 %1, label %foo, label %baz

foo:
	%2 = fcmp oeq double 3.0, 4.0
	br i1 %2, label %bar, label %baz

bar:
	br label %baz

baz:
	%3 = phi i32 [ 10, %foo ], [ 20, %bar ], [ 30, %baz ]
	%4 = select i1 true, i32 11, i32 22
	%5 = call i32 @g()
	call void @h(i32 30)
	%6 = va_arg i8* null, i32
	%7 = landingpad { i8*, i32 }
		catch i8** null
	ret void

handler0:
	%8 = catchpad within %cs [i8** null]
	ret void

handler1:
	%9 = cleanuppad within %cs [i8** null]
	ret void

dispatch:
	%cs = catchswitch within none [label %handler0, label %handler1] unwind to caller
}
