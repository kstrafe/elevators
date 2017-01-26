package main

import (
	"fmt"
	"runtime"
	"sync"
	"time"
)

var i int
var mutex = &sync.Mutex {}

func add1(number int) int {
	return number + 1
}

func sub1(number int) int {
	return number - 1
}

func thread_function(functor func(int) int) {
	for j := 0; j < 1000000; j++ {
		mutex.Lock()
		i = functor(i)
		mutex.Unlock()
	}
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	go thread_function(add1)
	go thread_function(sub1)
	time.Sleep(100*time.Millisecond)
	fmt.Println(i)
}
