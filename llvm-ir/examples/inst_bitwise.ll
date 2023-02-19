define void @f() {
0:
	%1 = shl i32 1, 2
	%2 = lshr i32 3, 4
	%3 = ashr i32 5, 6
	%4 = and i32 7, 8
	%5 = or i32 9, 10
	%6 = xor i32 11, 12
	ret void
}
