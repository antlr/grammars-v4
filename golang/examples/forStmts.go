package samples

import "fmt"

func S() (int, int) {
	return 1, 2
}

func ForStmts() {
	a, b := 1, 2
	for a < b {
		fmt.Println("From condition-only ForStmt")
		break
	}

	for ;; {
		fmt.Println("From empty ForClause ForStmt")
		break
	}

	// non-compaliable
	/*for j: = 0 ; ; {
        a += b
		fmt.Println("From ForClause with only init ForStmt")
		break
	}*/

	for ; a < b; {
		fmt.Println("From condition-only ForClause ForClause ForStmt")
		break
	}

	for j:=0 ; a < b; {
		j++
		fmt.Println("From init and condition ForClause ForClause ForStmt")
		break
	}

	for j:=0 ; ; j++ {
		fmt.Println("From init and post ForClause ForClause ForStmt")
		break
	}

	for  ; a < b ; a++ {
		fmt.Println("From condition and post ForClause ForClause ForStmt")
		break
	}

	for z:=0 ; a < b ; z++ {
		fmt.Println("From full ForClause ForClause ForStmt")
		return
	}
}
