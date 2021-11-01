package c

import "golang.org/x/tools/internal/lsp/rename/b"

func _() {
	b.Hello() //@rename("Hello", "Goodbye")
}

func ahead() {
	b.Hello() //@rename("Hello", "Goodbye")
	b.Hello() //@rename("Hello", "Goodbye")
}
