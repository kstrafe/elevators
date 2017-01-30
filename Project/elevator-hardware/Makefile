# Which compiler to use
CC = gcc

# Compiler flags go here.
CFLAGS = -std=gnu11 -g -Wall -Wextra -fPIC

all:
	$(CC) $(CFLAGS) -c io.c -o io.o
	$(CC) $(CFLAGS) -c elev.c -o elev.o
	$(CC) -shared -Wl,-soname,libelevator-hardware.so.1 -o libelevator-hardware.so.1.0.1 *.o -lcomedi
	ln -s libelevator-hardware.so.1.0.1 libelevator-hardware.so

clean:
	rm -f *.o
	rm -f *.so*

rebuild:	clean all

.PHONY: all rebuild clean
