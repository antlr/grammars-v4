package parser

// Set is a type that represents a set implemented as a map
type Set[T comparable] struct {
    items map[T]struct{}
}

// NewSet creates and returns a new set
func NewSet[T comparable]() *Set[T] {
    return &Set[T]{items: make(map[T]struct{})}
}

// Add adds an element to the set
func (s *Set[T]) Add(element T) {
    s.items[element] = struct{}{}
}

// Remove removes an element from the set
func (s *Set[T]) Remove(element T) {
    delete(s.items, element)
}

// Contains checks if an element is in the set
func (s *Set[T]) Contains(element T) bool {
    _, exists := s.items[element]
    return exists
}

// Size returns the number of elements in the set
func (s *Set[T]) Size() int {
    return len(s.items)
}

// Clear removes all elements from the set
func (s *Set[T]) Clear() {
    s.items = make(map[T]struct{})
}

// Elements returns all elements in the set as a slice
func (s *Set[T]) Elements() []T {
    elements := make([]T, 0, len(s.items))
    for key := range s.items {
        elements = append(elements, key)
    }
    return elements
}
