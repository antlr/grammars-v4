package samples

type Devnull struct {}

func noResult() {
	return
}

func simpleF() int {
	return 2
}

func complexF1() (re float64, im float64) {
	return -7.0, -4.0
}

func complexF2() (re float64, im float64) {
	return complexF1()
}

func complexF3() (re float64, im float64) {
	re = 7.0
	im = 4.0
	return
}

func (Devnull) writeLength(p []byte) (n int, _ error) {
	n = len(p)
	return
}

func ReturnImported() {
	var p []byte
	var devnull Devnull
	noResult() // +
	simpleF() // + 
	complexF1() // +
	complexF2() // + but sort out with propogating multiRet
	complexF3() // -
	devnull.writeLength(p) // -
}