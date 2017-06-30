# Makefile for utilities library
# includes: hash table manipulation
#           list processing
# Copyright (c) 1995-1996 G. Janssen

#CC = xlc

# specify appropriate C compiler flags here:
# include -DDO_CHECKS to incorporate range checks in bitvector routines
#CFLAGS= -O -D_POSIX_SOURCE
CFLAGS= -O

obj=	list.o hash.o alloc.o

utils : libutils.a

libutils.a : ${obj}
	ar r libutils.a ${obj}
	ranlib libutils.a

.c.o : ; $(CC) ${CFLAGS} -c $*.c

list.o : list.c list.h alloc.h
hash.o : hash.c hash.h alloc.h
alloc.o : alloc.c

install: utils
	cp *.h /usr/es/include/utils
	cp *.a /usr/es/lib

clean : 
	rm -f *.o *.a .pure

what :
	# make       - makes the archive libutils.a
	# make utils - makes the archive libutils.a
	# make clean - cleans up objects and archive in this directory
