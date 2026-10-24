@s = constant [4 x i8] c"foo\00"

define void @f() {
0:
	%ptr = alloca i32
	%1 = load i32, i32* %ptr
	store i32 42, i32* %ptr
	fence acquire
	%2 = cmpxchg i32* %ptr, i32 10, i32 20 acquire monotonic
	%3 = atomicrmw add i32* %ptr, i32 30 acq_rel
	%4 = getelementptr [4 x i8], [4 x i8]* @s, i64 0, i64 0
	ret void
}
