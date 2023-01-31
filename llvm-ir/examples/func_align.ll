$g = comdat any
$h = comdat any

define void @f() align 2 {
0:
	ret void
}

define void @g() align 2 comdat {
; <label>:0
	ret void
}

define void @h() comdat align 2 {
; <label>:0
	ret void
}
