# Which compiler to use
CC = gcc

# Compiler flags go here.
CFLAGS = -std=gnu11 -g -Wall -Wextra -fPIC

# Linker flags go here.
LDFLAGS = -lcomedi -lm

# list of sources
ELEVSRC = elev.c io.c main.c

# program executable file name.
TARGET = elevator

# top-level rule, to compile everything.
all: $(TARGET)

# Define all object files.
ELEVOBJ = $(ELEVSRC:.c=.o)

# rule to link the program
$(TARGET): $(ELEVOBJ)
	$(CC) $^ -o $@ $(LDFLAGS)

# Compile: create object files from C source files.
%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY:
make-lib:
	gcc -shared -Wl,-soname,libelevator-hardware.so.1 -o libelevator-hardware.so.1.0.1 io.o elev.o -lcomedi
	ln -s libelevator-hardware.so.1.0.1 libelevator-hardware.so

# rule for cleaning re-compilable files.
clean:
	rm -f $(TARGET) $(ELEVOBJ)
	rm -f *.so*

rebuild:	clean all

.PHONY: all rebuild clean
