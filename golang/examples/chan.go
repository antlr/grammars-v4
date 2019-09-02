package samples

import (
	"fmt"
	"time"
)

func Ch() {
	var ball = make(chan string)
	kickBall := func(playerName string) {
		for {
			fmt.Println(<-ball, "kicked the ball.")
			time.Sleep(time.Second)
			ball <- playerName
		}
	}
	go kickBall("John")
	go kickBall("Alice")
	go kickBall("Bob")
	go kickBall("Emily")
	time.Sleep(time.Second * 2)
	ball <- "referee" // kick off

	var c chan bool // nil
	<-c               // blocking here for ever
}
