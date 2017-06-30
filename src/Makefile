# Copyright (c) 1995-1998 G. Janssen

BIN=$(HOME)/bin
UTILS=../utils

INCLUDES   = -I. -I$(UTILS)
LEXERGEN = flex
LEXERLIB =-ll
PARSERGEN = bison

ARS = $(UTILS)/libutils.a
#CC = xlc

# Specify appropriate C compiler flags here:
# Define FUN_STDDEFS as string to load initial file other than
# the default as specified in fun.c
FUN_STDDEFS = \"$(HOME)/lib/stddefs.fun\"
#CFLAGS= -O -D_POSIX_SOURCE -DFUN_STDDEFS=$(FUN_STDDEFS) $(INCLUDES)
# Define FIXED_SPINE_STACK if using fixed size spine stack.
#
# WARNING: Cannot use -O2 or -O3 when using 4-byte aligned long longs!
# works with -mdisable-fpregs though
#CFLAGS= -O -D_POSIX_SOURCE $(INCLUDES)
CFLAGS= -O -DFUN_STDDEFS=$(FUN_STDDEFS) $(INCLUDES)
LDFLAGS= -s

obj=  main.o fun.tab.o lex.yy.o fun.o compile.o reduce.o print.o scc.o cell.o

fun : ${obj}
	$(CC) ${LDFLAGS} -o fun ${obj} $(ARS) $(LEXERLIB) -lm

pure : ${obj}
	purify $(CC) -o fun.pure ${obj} $(ARS) $(LEXERLIB) -lm

.SUFFIXES:

.SUFFIXES: .c .o

.c.o : ; $(CC) ${CFLAGS} -c $*.c

main.o : main.c fun.h cell.h
fun.o : fun.c fun.h print.h cell.h
cell.o : cell.c cell.h
reduce.o : reduce.c reduce.h print.h cell.h
compile.o : compile.c compile.h cell.h
scc.o : scc.c scc.h cell.h
print.o : print.c print.h cell.h
fun.tab.o : fun.tab.c fun.h cell.h
fun.tab.c : fun.y fun.h cell.h
#	# Don't worry about the 17 shift/reduce conflicts!
	$(PARSERGEN) -d fun.y
fun.tab.h : fun.y fun.h cell.h
#	# Don't worry about the 17 shift/reduce conflicts!
	$(PARSERGEN) -d fun.y
lex.yy.o : lex.yy.c fun.tab.h fun.h cell.h
	$(CC) ${CFLAGS} -w -c lex.yy.c
lex.yy.c : fun.l fun.h cell.h
	$(LEXERGEN) -l fun.l

install : fun
	cp stddefs.fun ${HOME}/lib
	mv fun ${BIN}

clean : 
	rm -f *.o *.a lex.yy.c fun.tab.c fun.tab.h y.output fun fun.pure .pure

what :
	# make       - makes the program
	# make clean - cleans up objects and archives in this directory
