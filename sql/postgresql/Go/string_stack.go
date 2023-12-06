/*
PostgreSQL grammar.
The MIT License (MIT).
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

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
