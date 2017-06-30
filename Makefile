# Makefile for FUN
# Copyright (c) 1995-1996 G. Janssen

CC=gcc

all:
	cd utils; make CC="$(CC)"
	cd src; make CC="$(CC)"

clean:
	cd utils; make clean
	cd src; make clean
