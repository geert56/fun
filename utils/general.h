/* Universally useful types and constants. */

/* Copyright (c) 1997-1999 G. Janssen */

#ifndef GENERAL_H
#define GENERAL_H

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* Use as: "forever { ... }" or "do { ... } forever;" */
#define forever			while(1)
/* Use as: "do { ... } once;" */
#define once			while(0)

/* Number of bits in a Nat (natural number): */
#define NAT_BIT			32

/* What's the max unsigned integer (positive) value that fits in `n' bits: */
#define UBITS_MAX(n)		(~(-1 << (n)))

/* What's the min signed integer (negative) value that fits in `n' bits: */
#define SBITS_MIN(n)		(-1 << ((n)-1))

/* What's the max signed integer (positive) value that fits in `n' bits: */
#define SBITS_MAX(n)		(~SBITS_MIN(n))

/* Min and max values of various types: */
#define TINYINT_MIN		SCHAR_MIN
#define TINYINT_MAX		SCHAR_MAX
#define SMALLINT_MIN		SSHRT_MIN
#define SMALLINT_MAX		SSHRT_MAX
#define TINYNAT_MAX		UCHAR_MAX
#define SMALLNAT_MAX		USHRT_MAX
#define NAT_MAX			UINT_MAX
#define REAL_MAX		FLT_MAX

#define odd(n)			((Nat) (n) & 1)

#define min(a,b)		((a) < (b) ? (a) : (b))
#define max(a,b)		((a) > (b) ? (a) : (b))

/* Ceiling of a/b, a and b integer: */
/* (a + b - 1) / b */

/* The Nat value n*2^k: */
#define mul_pow2(n,k)		((Nat) (n) << (k))
/* The Nat value 2^n: */
#define pow2(n)			mul_pow2(1,(n))
/* The Nat value 2*n: */
#define mul2(n)			mul_pow2((n),1)
/* The Nat value n div 2^k: */
#define div_pow2(n,k)		((Nat) (n) >> (k))
/* The Nat value n div 2: */
#define div2(n)			div_pow2((n),1)
/* The Nat value n^2: */
#define sqr(x)			((x) * (x))

/* Swaps t typed values of vars a and b: */
#define swap(t,a,b)		do {\
				  t c_ = (a);\
				  (a)  = (b);\
				  (b)  = c_;\
				} once

#ifndef Strdup
#define Strdup(s)		(strcpy(malloc(strlen(s)+1), (s)))
#endif

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

/* Xlib.h seems to define this! */
#ifdef Bool
#undef Bool
#endif

typedef   signed char		TinyInt;
typedef    short int		SmallInt;
typedef          int		Int;
typedef unsigned char		Bool;
typedef unsigned char		Byte;
typedef unsigned char		TinyNat;
typedef unsigned short int	SmallNat;
typedef unsigned int		Nat;
typedef float			Real;
typedef char *			String;

#endif /* GENERAL_H */
