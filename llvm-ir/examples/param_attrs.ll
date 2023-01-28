%struct.T = type { i8, i32 }

define void @f(%struct.T* byval(%struct.T) align 4 %0) {
1:
	ret void
}

define void @g(%struct.T* byval align 4 %0) {
1:
	ret void
}
