P = main
LDFLAGS=
CFLAGS=-g -Wall -Werror
CC=gcc
OBJECTS=main.o qm.o q.o ops.o parser.o

$(P): $(OBJECTS)
