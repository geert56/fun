/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : cell.h
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1998 G.L.J.M. Janssen
 date	   :  6-JAN-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef CELL_H
#define CELL_H

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include "alloc.h"
#include "list.h"
#include "hash.h"
#include "general.h"

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* Define this if you have support for long longs (64-bit integers): */
#define LONGLONG_INTS

#ifdef LONGLONG_INTS
/* Define this if long long may be aligned on 4 byte boundaries,
   otherwise your compiler will probably treat struct L_EXPR larger
   than necessary.
*/
#define LONGLONG_ALIGN4

#define BITSPERINT		64
#else
#define BITSPERINT		32
#endif

/* Assuming 2-complement representation for signed integers! */
#define MIN_INT			((IntT) 1 << BITSPERINT-1)
#define MAX_INT			(~MIN_INT)

/* L_*: Possible type (tag) values for cell (2-bits): */
#define L_ABS_T			0 /* abstraction */
#define L_COMB_T		0 /* combinator */
#define L_APP_T			1 /* application */
#define L_PF_T			2 /* predefined (built-in) function */
#define L_vE_T			3 /* v <- E and v = E */
/* Data types (a bit each; total 7): */
#define L_CONS_T		4 /* list: [...] */
#define L_FILE_T	        8 /* file: */
#define L_NAM_T		       16 /* variable name: v */
#define L_BOOL_T	       32 /* boolean: False, True */
#define L_CHAR_T	       64 /* character: 'A' */ 
#define L_INT_T		      128 /* integer: 42 */
#define L_FLT_T		      256 /* floating-point: 3.14 */
/* Derived types: */
#define L_NUM_T			(L_INT_T|L_FLT_T)
#define L_ORD_T			(L_NAM_T|L_BOOL_T|L_CHAR_T|L_NUM_T)
#define L_ANY_T			(L_CONS_T|L_FILE_T|L_ORD_T)

/* PF_*: Built-in functions (value goes in aux field): */
#define PF_FIRST		 0
#define PF_NOT			 0
#define PF_POW			 1
#define PF_TIMES		 2
#define PF_DIVIDE		 3
#define PF_MODULO		 4
#define PF_PLUS			 5
#define PF_MINUS		 6
#define PF_SHIFTL		 7
#define PF_SHIFTR		 8
#define PF_LT			 9
#define PF_LE			10
#define PF_GT			11
#define PF_GE			12
#define PF_EQ			13
#define PF_NE			14
#define PF_AND			15
#define PF_OR			16
#define PF_IMPLIES		17
#define PF_IF			18
#define PF_CONS			19
#define PF_CAR			20
#define PF_CDR			21
#define PF_NULL			22
#define PF_LEN			23
#define PF_ITH			24
#define PF_SEQ			25
#define PF_FORCE		26
#define PF_APPEND		27
#define PF_TIME			28
#define PF_READ			29
#define PF_WRITE		30
#define PF_SHOWS		31
#define PF_STDOUT		32
#define PF_STDERR		33
#define PF_STDIN		34
#define PF_ERROR		35
#define PF_SIN			36
#define PF_COS			37
#define PF_TAN			38
#define PF_ASIN			39
#define PF_ACOS			40
#define PF_ATAN			41
#define PF_SINH			42
#define PF_COSH			43
#define PF_TANH			44
#define PF_ASINH		45
#define PF_ACOSH		46
#define PF_ATANH		47
#define PF_EXP			48
#define PF_LOG			49
#define PF_LOG10		50
#define PF_LOG2			51
#define PF_SQRT			52
#define PF_CEIL			53
#define PF_ABS			54
#define PF_FLOOR		55
#define PF_TYPE			56
#define PF_LAST_USER		56
/* These are invisible to a user: */
#define PF_PUTSTD		57
#define PF_GETC			58
#define PF_PUTC			59
#define PF_LAST			59
/* Name and arity of built-in functions PF_*: */
#define PF_STR(i)		pf_str[i]
#define PF_ARITY(i)		pf_arity[i]

/* *_COMB: Combinators that are generated by SK-compiler
   (coded in comb field; type field must be L_COMB_T):
*/
#define  UNUSED_COMB		0
#define  I_COMB			1
#define  K_COMB			2
#define  S_COMB			3
#define  B_COMB			4
#define  C_COMB			5
#define Sp_COMB			6
#define Bs_COMB			7
#define Cp_COMB			8
#define  Y_COMB			9
/* Name and arity of combinators *_COMB: */
#define COMB_STR(c)		comb_str[c]
#define COMB_ARITY(c)		comb_arity[c]

/* L_*: Access macros to L_EXPR fields: */
#define L_MARK1(L)		((L)->mark1)
#define L_MARK2(L)		((L)->mark2)
#define L_MARK3(L)		((L)->mark3)
#define L_TYPE(L)		((L)->type)
#define L_AUX(L)		((L)->aux)
#define L_PF(L)			L_AUX(L)
#define L_NAME(L)		((L)->u.nam.name)
#define L_NAME_MARK(L)		L_AUX(L)
#define L_NAME_EXTRA(L)		((L)->u.nam.extra)
#define L_COMB(L)		((L)->comb)
/* For now representing BOOL and CHAR as INT!
#define L_BOOL(L)		((L)->u.Bool)
#define L_CHAR(L)		((L)->u.Char)
*/
#define L_BOOL(L)		L_INT(L)
#define L_CHAR(L)		L_INT(L)
#ifdef LONGLONG_ALIGN4
/* Dirty trick! Only used to force a 12-byte struct L_EXPR. */
#define L_INT(L)		(*((IntT *) &((L)->u.Num)))
#else
#define L_INT(L)		((L)->u.Num)
#endif
#define L_FLT(L)		((L)->u.Float)
#define L_CAR(L)		L_RATOR(L)
#define L_CDR(L)		L_RAND (L)
#define L_FST(L)		L_RATOR(L)
#define L_SND(L)		L_RAND (L)
#define L_RATOR(L)		((L)->u.app.rator)
#define L_RAND(L)		((L)->u.app.rand)
#define L_vE_v(L)		((L)->u.vE.v)
#define L_vE_E(L)		((L)->u.vE.E)
#define L_BV(L)			L_vE_v(L)
#define L_BODY(L)		L_vE_E(L)
#define L_FILE_NAM(L)		((L)->u.file.fname)
#define L_FILE_FP(L)		((L)->u.file.fp)
#define L_NEXT(L)		((L)->u.next)

/* L_*_P: Predicates on L_EXPRs: */
#define L_ABS_P(L)		(L_TYPE(L) == L_ABS_T)
#define L_COMB_P(L)		(L_TYPE(L) == L_COMB_T)
#define L_APP_P(L)		(L_TYPE(L) == L_APP_T)
#define L_PF_P(L)		(L_TYPE(L) == L_PF_T)
#define L_NAM_P(L)		(L_TYPE(L) == L_NAM_T)
#define L_BOOL_P(L)		(L_TYPE(L) == L_BOOL_T)
#define L_CHAR_P(L)		(L_TYPE(L) == L_CHAR_T)
#define L_INT_P(L)		(L_TYPE(L) == L_INT_T)
#define L_FLT_P(L)		(L_TYPE(L) == L_FLT_T)
#define L_NUM_P(L)		(L_TYPE(L) & L_NUM_T)
#define L_CONS_P(L)		(L_TYPE(L) == L_CONS_T)
#define L_NIL_P(L)		L_PF(L)
#define L_vE_P(L)		(L_TYPE(L) == L_vE_T)
#define L_FILE_P(L)		(L_TYPE(L) == L_FILE_T)

/* L_*_P: Predicates on combinators (assumes L_COMB_P(L)): */
#define L_I_P(L)		(L_COMB(L) ==  I_COMB)
#define L_K_P(L)		(L_COMB(L) ==  K_COMB)
#define L_S_P(L)		(L_COMB(L) ==  S_COMB)
#define L_B_P(L)		(L_COMB(L) ==  B_COMB)
#define L_C_P(L)		(L_COMB(L) ==  C_COMB)
#define L_Sp_P(L)		(L_COMB(L) == Sp_COMB)
#define L_Bs_P(L)		(L_COMB(L) == Bs_COMB)
#define L_Cp_P(L)		(L_COMB(L) == Cp_COMB)
#define L_Y_P(L)		(L_COMB(L) ==  Y_COMB)

/* Used to protect cells during gc marking phase. */
#define L_GCFLAGGED(E)		L_MARK1(E)
#define L_SET_GCFLAG(E)		(L_MARK1(E) = 1)
#define L_RESET_GCFLAG(E)	(L_MARK1(E) = 0)

/* Used to flag substituted global definitions during compilation. */
#define L_CFLAGGED(E)		L_MARK2(E)
#define L_SET_CFLAG(E)		(L_MARK2(E) = 1)
#define L_RESET_CFLAG(E)	(L_MARK2(E) = 0)

/* Used to detect cyclic structures during printing. */
#define L_PFLAGGED(E)		L_MARK3(E)
#define L_SET_PFLAG(E)		(L_MARK3(E) = 1)
#define L_RESET_PFLAG(E)	(L_MARK3(E) = 0)

/* Short-hands for abstraction constructors: */
#define ABS1(v1,B)		mk_abs_cell(v1, B)
#define ABS2(v1,v2,B)		mk_abs_cell(v1, ABS1(v2, B))
#define ABS3(v1,v2,v3,B)	mk_abs_cell(v1, ABS2(v2, v3, B))
#define ABS4(v1,v2,v3,v4,B)	mk_abs_cell(v1, ABS3(v2, v3, v4, B))

/* Short-hands for application constructors: */
#define APP1(E1)		(E1)
#define APP2(E1,E2)		mk_app_cell(APP1(E1), E2)
#define APP3(E1,E2,E3)		mk_app_cell(APP2(E1, E2), E3)
#define APP4(E1,E2,E3,E4)	mk_app_cell(APP3(E1, E2, E3), E4)
#define APP5(E1,E2,E3,E4,E5)	mk_app_cell(APP4(E1, E2, E3, E4), E5)

/* Easy construction of explicit input expression: */
#define IF(B,T,E)		APP4(L_if,   B, T, E)
#define CONS(F,R)		APP3(L_Cons, F, R)
#define CAR(E)			APP2(L_car,  E)
#define CDR(E)			APP2(L_cdr,  E)
#define NILP(E)			APP2(L_null, E)
#define COMP(E1,E2)		S_APP(K_APP(E1), E2)
/* Allow optimise option to work properly in all cases.
#define COMP(E1,E2)		B_APP(E1,E2)
*/
#define APPEND(E1,E2)		APP3(L_append, E1, E2)
#define PLUS(E1,E2)		APP3(L_plus,   E1, E2)
#define MINUS(E1,E2)		APP3(L_minus,  E1, E2)
#define GT(E1,E2)		APP3(L_gt,     E1, E2)

/* Macros used by SK-compiler: */
/* I */
#define I_APP()			I_comb
/* (K E) */
#define K_APP(E)		APP2(K_comb, E)
/* ((S E1) E2) */
#define S_APP(E1, E2)		APP3(S_comb, E1, E2)
/* ((B E1) E2), optimise ((B E1) I to E1 */
#define B_APP(E1, E2)		(L_I_P(E2) ?(E1) : APP3(B_comb, E1, E2))
/*#define B_APP(E1, E2)		APP3(B_comb, E1, E2)*/
/* ((C E1) E2) */
#define C_APP(E1, E2)		APP3(C_comb, E1, E2)
/* (((S' E1) E2) E3) */
#define Sp_APP(E1, E2, E3)	APP4(Sp_comb, E1, E2, E3)
/* (((B* E1) E2) E3) */
#define Bs_APP(E1, E2, E3)	APP4(Bs_comb, E1, E2, E3)
/* (((C' E1) E2) E3) */
#define Cp_APP(E1, E2, E3)	APP4(Cp_comb, E1, E2, E3)
/* (Y E) */
#define Y_APP(E)		APP2(Y_comb, E)

/* Declare a gcpro struct variable: */
#define DCL_GCPRO1		struct gcpro gcpro1

/* Protect an L_EXPR cell.
   (Make sure E has some valid L_EXPR as value when gc occurs!)
*/
#define GCPRO1(E) \
  do { \
    gcpro1.next = gcprolist; gcpro1.var = &E; gcprolist = &gcpro1; \
  } once

/* Release the protection of an L_EXPR cell: */
#define UNGCPRO()		(gcprolist = gcpro1.next)

/* Number of bytes allocated per block of cell storage: */
#define SIZEOF_BLOCK		pow2(17)

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

/* Type long long is non-portable! */
#ifdef LONGLONG_INTS
typedef long long int IntT;
typedef unsigned long long int UIntT;
#else
/* Use these instead: */
typedef int IntT;
typedef unsigned int UIntT;
#endif

typedef struct L_EXPR *L_EXPR;

/* Upon creation all cells are guaranteed to be empty, i.e., its
   fields are 0, or '\0', or NULL.
*/
struct L_EXPR {
  unsigned int mark1  :  1;
  unsigned int mark2  :  1;
  unsigned int mark3  :  1;
  unsigned int comb   :  4;
  unsigned int type   :  9;	/* L_COMB_T <==> comb > 0 */
  unsigned int  aux   : 16;	/* PF / NAME_MARK */
  union {
    /* L_COMB_T, L_PF_T */
    /* no further data */

    /* L_NAM_T */
    struct {
      const char *name;
      void *extra;		/* general purpose (used by scc.c) */
    } nam;

    /* L_BOOL_T; not yet used. */
/*    int Bool;*/

    /* L_CHAR_T; not yet used. */
/*    int Char;*/

    /* L_INT_T; for now also L_BOOL_T and L_CHAR_T. */
#ifdef LONGLONG_ALIGN4
/* Dirty trick! Only used to force a 12-byte struct L_EXPR. */
    int Num;
#else
    IntT Num;
#endif

    double Float;

    /* L_APP_T, L_CONS_T */
    /* (rator rand) also misused for CONS cell [car:cdr]. */
    struct {
      L_EXPR rator;
      L_EXPR rand;
    } app;

    /* L_ABS_T, L_vE_T(L v . E, also v <- E, and v = E) */
    struct {
      const char *v;
      L_EXPR E;
    } vE;

    /* L_FILE_T(Argument for PF_GETC) */
    struct {
      const char *fname;
      FILE *fp;
    } file;

    /* Used for memory management: */
    L_EXPR next;
  } u;
};

/* Structure for recording C stack slots that need marking.
   This is a chain of structures, each of which points at a L_EXPR
   variable whose value should be marked in garbage collection.
   Normally every link of the chain is an automatic variable of a function,
   and its `val' points to some argument or local variable of the function.
   On exit to the function, the chain is set back to the value it had on entry.
   This way, no link remains in the chain when the stack frame containing the
   link disappears.
*/
struct gcpro {
  L_EXPR *var;			/* Address of protected variable */
  struct gcpro *next;
};

/* ------------------------------------------------------------------------ */
/* VARIABLES                                                                */
/* ------------------------------------------------------------------------ */

/* Number of cells allocated at one time: */
extern Nat nr_cells_allocated;
extern Nat nr_cells_in_use;
extern Nat nr_blocks_allocated;
extern Nat nr_gc_calls;

/* Unique cells, not to be garbage-collected: */
extern L_EXPR OperatorsTable[];

/* Global definitions table; Always protected from garbage-collection: */
extern HASHTAB *GlobalDefsTable;

/* Unique cells, not to be garbage-collected: */
extern L_EXPR  I_comb;
extern L_EXPR  K_comb;
extern L_EXPR  S_comb;
extern L_EXPR  B_comb;
extern L_EXPR  C_comb;
extern L_EXPR Sp_comb;
extern L_EXPR Bs_comb;
extern L_EXPR Cp_comb;
extern L_EXPR  Y_comb;

extern L_EXPR L_null;
extern L_EXPR L_len;
extern L_EXPR L_car;
extern L_EXPR L_cdr;
extern L_EXPR L_Cons;
extern L_EXPR L_append;
extern L_EXPR L_if;
extern L_EXPR L_plus;
extern L_EXPR L_minus;
extern L_EXPR L_lt;
extern L_EXPR L_le;
extern L_EXPR L_ge;
extern L_EXPR L_gt;

extern L_EXPR L_Nil;
extern L_EXPR L_False;
extern L_EXPR L_True;
extern L_EXPR L_0;
extern L_EXPR L_1;
extern L_EXPR L_Stddefs;
extern L_EXPR L_Builtins;
extern L_EXPR L_Argv;

/* Name strings of combinators: */
extern const char *const comb_str[];

/* Number of arguments required for each combinator: */
extern const unsigned char comb_arity[];

/* Name strings of built-ins: */
extern const char *const pf_str[];

/* Number of args expected for each built-in: */
extern const unsigned char pf_arity[];

extern struct gcpro *gcprolist;

/* We assume that parsing and compilation don't generate too much cells.
   Therefore gc will only be on during reduction.
*/
extern Bool gc_on;
extern Bool gc_in_progress;

extern Bool cell_verbose;
extern Bool cell_debug;

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

#ifdef LONGLONG_INTS
extern IntT atol2(const char *s);
extern const char *l2toa(IntT num);
#else
#define atol2(s)	atol(s)
#define l2toa(n)	ltoa(n)
#endif
extern const char *dtoa(double num);

extern const char *Canonise (const char *str);
extern const char *CanoniseC(const char *str);

extern void gc(void);

/* Returns (compiled) definition for global name, or NULL if none present
   in GlobalDefsTable.
*/
extern L_EXPR get_def(const char *name);

/*extern void set_def(const char *v, L_EXPR E);*/

/* Returns cell for the named symbol or NULL if none present in NamesTable.
   Assumes name to be canonised.
*/
extern L_EXPR get_name(const char *name);

/* Returns new empty cell of specified type: */
extern L_EXPR mk_cell(int type);

extern void free_cell(L_EXPR E);

/* Returns new cell for the cons [car:cdr] */
extern L_EXPR mk_cons_cell(L_EXPR car, L_EXPR cdr);

/* Returns new cell for a file pointer fp with name fnam */
extern L_EXPR mk_file_cell(FILE *fp, const char *fnam);

/* Returns new cell for the application (E1 E2) */
extern L_EXPR mk_app_cell(L_EXPR E1, L_EXPR E2);

/* Returns new cell for the abstraction (L v . B) */
extern L_EXPR mk_abs_cell(const char *v, L_EXPR B);

/* Returns new cell for (v <- E), also used for v = E */
extern L_EXPR mk_vE_cell(const char *v, L_EXPR E);

/* Returns a new cell for the boolean (0 or 1). */
extern L_EXPR mk_bool_cell(int bool);

/* Returns a new cell for the character. */
extern L_EXPR mk_char_cell(char c);

/* Returns a new cell for the integer number. */
extern L_EXPR mk_num_cell(IntT num);

/* Returns a new cell for the float number. */
extern L_EXPR mk_float_cell(double num);

/* Returns cell for the named symbol. Assumes name to be canonised.
   Name cells are unique per name, i.e., we can't have different cells
   that refer to the same name string.
*/
extern L_EXPR mk_name_cell(const char *name);

/* Returns the (unique) cell for the specified built-in operator. */
extern L_EXPR mk_op_cell(int op);

/* Converts a C string `s' to a L_EXPR list of its constituent characters.
   Uses `last_cdr' arg as value for cdr field of last cons cell.
   Note: for empty string `s' returns `last_cdr' (usually L_Nil).
   Watch out for gc! It will be switched off.
*/
extern L_EXPR mk_str(const char *s, L_EXPR last_cdr);

/* Copies top-level of list `E', using `cdr' as cdr value of last new cons
   cell. If `cdr' = NULL then uses last cdr value of `E' instead.
   Watch out for gc! It will be switched off.
   Pre: L_CONS_P(E).
*/
extern L_EXPR cp_list(L_EXPR E, L_EXPR cdr);

/* Copies an expression. Leaf cells will be shared though. */
extern L_EXPR L_cp(L_EXPR E);

extern void cell_print_stats(FILE *fp);

extern void cell_init(void);

extern void cell_quit(void);

#endif /* CELL_H */
