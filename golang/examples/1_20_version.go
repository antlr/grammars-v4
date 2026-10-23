package onetwenty

type A[T comparable] struct {
	ComparableField T
}

type B = string

type Constraint interface {
	A[string] | ~B
}

func foo[T comparable](value T) A[T] {
	return A[T]{ComparableField: value}
}
