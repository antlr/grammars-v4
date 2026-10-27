@seed = global i32 0

declare i32 @abs(i32 %x)

define i32 @rand() {
0:
	%1 = load i32, i32* @seed
	%2 = mul i32 %1, 22695477
	%3 = add i32 %2, 1
	store i32 %3, i32* @seed
	%4 = call i32 @abs(i32 %3)
	ret i32 %4
}
