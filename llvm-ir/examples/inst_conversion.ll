define void @f() {
0:
	%1 = trunc i32 321 to i8
	%2 = zext i8 123 to i32
	%3 = sext i8 -123 to i32
	%4 = fptrunc double 1.0 to float
	%5 = fpext float 2.0 to double
	%6 = fptoui double 3.0 to i32
	%7 = fptosi double -4.0 to i32
	%8 = uitofp i32 5 to double
	%9 = sitofp i32 -6 to double
	%10 = ptrtoint i8* null to i32
	%11 = inttoptr i32 1234 to i8*
	%12 = bitcast { i32, i32 }* null to i64*
	%13 = addrspacecast i8* null to i8 addrspace(1)*
	ret void
}
