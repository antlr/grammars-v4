define void @f() {
0:
	%1 = extractelement <2 x i32> <i32 1, i32 2>, i64 1
	%2 = insertelement <2 x i32> <i32 4, i32 6>, i32 5, i64 1
	%3 = shufflevector <2 x i32> <i32 7, i32 8>, <2 x i32> <i32 9, i32 10>, <4 x i32> <i32 3, i32 2, i32 1, i32 0>
	ret void
}
