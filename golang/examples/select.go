package samples

func Selects() {
	var a []int
	var c, c1, c2, c3, c4, c5 chan int
	var i1, i2 int
	select {
	case i1 = <-c1:
		print("received ", i1, " from c1\n")
	case c2 <- i2:
		print("sent ", i2, " to c2\n")
	case i3, ok := (<-c3): // same as: i3, ok := <-c3
		if ok {
			print("received ", i3, " from c3\n")
		} else {
			print("c3 is closed\n")
		}
	case <-c5:
		print("received without an assignment")
	case a[f()] = <-c4:
		// same as:
		// case t := <-c4
		//	a[f()] = t
	default:
		print("no communication\n")
	}

	for i := 0; i < 10; i++ { // send random sequence of bits to c
		select {
		case c <- 0: // note: no statement, no fallthrough, no folding of cases
		case c <- 1:
		default:
		}
	}

}
