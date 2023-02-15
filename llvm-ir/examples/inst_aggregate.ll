define void @f() {
0:
	%1 = extractvalue { i8, { i32, i64 } } { i8 1, { i32, i64 } { i32 2, i64 3 } }, 1, 1
	%2 = insertvalue { i8, { i32, i64 } } { i8 1, { i32, i64 } { i32 2, i64 3 } }, i64 4, 1, 1
	ret void
}
