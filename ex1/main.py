#! /usr/bin/env python3
from threading import Thread

i = 0

def add1(number):
	return number + 1
def sub1(number):
	return number - 1

def thread_function(functor):
	global i
	for j in range(1000000):
		i = functor(i)

def main():
	thread_1 = Thread(target = thread_function, args = [add1])
	thread_2 = Thread(target = thread_function, args = [sub1])
	thread_1.start()
	thread_2.start()
	thread_1.join()
	thread_2.join()
	print(i)

main()
