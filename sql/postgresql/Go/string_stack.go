package parser

import (
	"errors"
)

var (
	ErrorStackEmpty = errors.New("stack empty")
)

type StringStack struct {
	items []string
}

func (receiver *StringStack) Push(value string) {
	receiver.items = append(receiver.items, value)
}

func (receiver *StringStack) Pop() (string, error) {
	if receiver.IsEmpty() {
		return "", ErrorStackEmpty
	}
	value := receiver.items[0]
	receiver.items = receiver.items[1:]
	return value, nil
}

func (receiver *StringStack) PopOrEmpty() string {
	value, err := receiver.Pop()
	if err != nil {
		return ""
	}
	return value
}

func (receiver *StringStack) Peek() (string, error) {
	if receiver.IsEmpty() {
		return "", ErrorStackEmpty
	}
	return receiver.items[0], nil
}

func (receiver *StringStack) PeekOrEmpty() string {
	value, err := receiver.Peek()
	if err != nil {
		return ""
	}
	return value
}

func (receiver *StringStack) Size() int {
	return len(receiver.items)
}

func (receiver *StringStack) IsEmpty() bool {
	return receiver.Size() == 0
}
