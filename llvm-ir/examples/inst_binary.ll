define void @f() {
0:
	%1 = add i32 1, 2
	%2 = fadd double 3.0, 4.0
	%3 = sub i32 5, 6
	%4 = fsub double 7.0, 8.0
	%5 = mul i32 9, 10
	%6 = fmul double 11.0, 12.0
	%7 = udiv i32 13, 14
	%8 = sdiv i32 15, 16
	%9 = fdiv double 17.0, 18.0
	%10 = urem i32 19, 20
	%11 = srem i32 21, 22
	%12 = frem double 23.0, 24.0
	ret void
}
