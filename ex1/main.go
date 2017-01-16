package main

import (
	"fmt"
	"runtime"
	"time"
)

var i int

func add1(number int) int {
	return number + 1
}

func sub1(number int) int {
	return number - 1
}

func thread_function(functor func(int) int) {
	for j := 0; j < 1000000; j++ {
		i = functor(i)
	}
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	go thread_function(add1)
	go thread_function(sub1)
	time.Sleep(100*time.Millisecond)
	fmt.Println(i)
}
