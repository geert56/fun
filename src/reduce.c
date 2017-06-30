/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : reduce.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1997 G.L.J.M. Janssen
 date	   :  8-DEC-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include <stdarg.h>
#include <math.h>
#include <time.h>
#include <setjmp.h>
#include <signal.h>

#include "cell.h"
#include "print.h"
#include "reduce.h"

/* ------------------------------------------------------------------------ */
/* IMPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

/* Define this if using fixed size spine stack: */
/*#define FIXED_SPINE_STACK*/

/* (Initial dynamic) reduction spine stack size: */
#define INIT_STACK_SIZE		pow2(10)

/* Maximum for nested reduce_lazy calls(to avoid C-stack overflow): */
#define MAX_REDUCE_LEVEL	pow2(14)

/* Push an L_EXPR onto the top of the spine stack: */
#define push(E)		do { \
			  if (top == stack_end) extend_stack(); \
			  *top++ =(E); \
			  if (top - spine_stack > stack_ceiling) \
                            stack_ceiling++; \
		        } once

/* Get the L_EXP at position i below the top of the spine stack:
   (below means toward the stack base; usually i > 0 because anything
   on top or above should be considered undefined!)
*/
#define peek_down(i)	(*(top - (i)))
/* Pop an L_EXPR from the spine stack(not used yet): */
#define pop()		(*--top)
/* Discard the top n L_EXPRs from the spine stack: */
#define drop_n(n)	(top -= (n))
/* Get current top position as index(for saving): */
#define save_top()	(top - spine_stack)
/* Discard absolute: reset top to r(normally obtained by save_top): */
#define restore_to(r)	(top = spine_stack + (r))
/* Number of items pushed since top was(index) r: */
#define nr_pushed(r)	(top -(spine_stack + (r)))
/* The i-th argument of a function, counted from 1 for first arg: */
#define A(i)		L_RAND(peek_down(i))

#define COERCE(A)	(L_FLT_P(A) ? L_FLT(A) : (double) L_INT(A))

/* Overwrites cell E with contents of cell A: */
#define L_OVERWRITE(E,A)	(*(E) = *(A))

/* Watch out: if x involves reduction then protection of E no longer
   also protects the graph part under it because its type has changed.
   So, if x uses cells `under' E it must protect those itself.
   We definitely don't want this (see below); currently this would only
   happen with PF_LEN, which therefore doesn't use the UPDATE macro.
   (UPDATEs are a form of OVERWRITEs).
*/
#define UPDATE_E_INT(x) \
	do { \
	  L_TYPE(E) = L_INT_T; \
	  L_INT (E) = (x); \
	} once

#define UPDATE_E_FLT(x) \
	do { \
	  L_TYPE(E) = L_FLT_T; \
	  L_FLT (E) = (x); \
	} once

#define UPDATE_E_BOOL(x) \
	do { \
	  L_TYPE(E) = L_BOOL_T; \
	  L_BOOL(E) = (x); \
	} once

/* L_OVERWRITEs of the root redex E make the old graph inaccessible.
   In most cases could therefore first `free' the old graph before
   the new one is created (via some reduction).
   Of course we cannot really free the old graph, but the least we can do
   is make it look unused to the garbage collector. This we do by temporary
   making it a constant node. This action leaves the graph in a inrepairable
   state in case of exceptional preemption of the reduction. Unless we make
   sure that reduction uses copies of global definitions, some definitions
   might kept corrupted when reduction is interrupted. Another solution is
   to gracefully unwind the recursive calls to reduce_lazy, but that's
   obviously quite intricate and simply too much work.
   Better is to insist on the two actions FREE_E followed by L_OVERWRITE
   to be considered atomic, i.e., interrupts may occur but are acted upon
   only afterwards.

   For now decided not to use this feature. Haven't found enough evidence
   to warrant all the trouble.
*/
/*#define FREE_E()	do { L_TYPE(E) = L_INT_T; } once*/
#define FREE_E()

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */

static int max_stack_size;
#ifdef FIXED_SPINE_STACK
/* Use this for fixed size spine stack: */
static L_EXPR spine_stack[INIT_STACK_SIZE];
#else
static L_EXPR *spine_stack;
#endif
static L_EXPR *stack_end;
/* top always refers to first free place on the spine stack,
   hence *top is undefined.
*/
static L_EXPR *top;

static Bool interrupted;
static jmp_buf Exception;

/* ------------------------------------------------------------------------ */
/* EXPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

int stack_ceiling  = 0;
int nr_reductions  = 0;
Bool reduce_verbose = 0;
Bool reduce_debug   = 0;

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

static L_EXPR L_reduce_lazy (L_EXPR E);
static L_EXPR L_reduce_force(L_EXPR E);
static    Nat L_reduce_seq  (L_EXPR E);

/* ------------------------------------------------------------------------ */
/* SK REDUCTOR                                                              */
/* ------------------------------------------------------------------------ */

#ifdef FIXED_SPINE_STACK
#define extend_stack()	exception("Reduction stack exhausted: %d items.\n", \
				  stack_ceiling)
#else
static void
extend_stack(void)
{
  int o_top_offset = save_top();
  int o_size = max_stack_size;

  max_stack_size *= 2;
  spine_stack = MA_REALLOC_ARRAY(spine_stack, max_stack_size, L_EXPR, o_size);
  stack_end = spine_stack + max_stack_size;
  restore_to(o_top_offset);
}

#if 0
/* Alternative. */
static void
extend_stack(void)
{
  int o_top_offset = save_top();
  int i, o_size = max_stack_size;
  L_EXPR *s, *t, *o_spine_stack = spine_stack;

  max_stack_size *= 2;
  fprintf(stderr, "Extending spine stack to %d items...", max_stack_size);
  spine_stack = MA_MALLOC_ARRAY(max_stack_size, L_EXPR);
  for (t = spine_stack, s = o_spine_stack, i = 0; i < o_size; i++) *t++ = *s++;
  MA_FREE_ARRAY(o_spine_stack, o_size, L_EXPR);
  stack_end = spine_stack + max_stack_size;
  restore_to(o_top_offset);
  fputs("done.\n", stderr);
}
#endif
#endif

/* Prints formatted message to stderr and raises exception. */
static void
exception(const char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  vfprintf(stderr, format, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  longjmp(Exception, 1);
}

static void
float_exception(int sig)
{
  interrupted = 1;
  print_message(NULL, "Floating exception.");
  signal(SIGFPE, float_exception);
}

static void
sigint_handler(int sig)
{
  interrupted = 1;
  signal(SIGINT, SIG_DFL);
}

#ifdef hpux
double
asinh(double x)
{
  return log(x + sqrt(x * x + 1));
}
double
acosh(double x)
{
  return log(x + sqrt(x * x - 1));
}
/* Pre: -1.0 < x < 1.0 */
double
atanh(double x)
{
  return 0.5 * log((1 + x) /(1 - x));
}
#endif

/* To be called only once. */
void
reduce_init(void)
{
  max_stack_size = INIT_STACK_SIZE;
#ifndef FIXED_SPINE_STACK
  spine_stack = MA_MALLOC_ARRAY(max_stack_size, L_EXPR);
#endif
  top = spine_stack;
  stack_end = spine_stack + max_stack_size;
  signal(SIGFPE, float_exception);
}

/* Returns whether `n' elements on stack starting with the argument A(i)
   can succesfully be reduced to a cell belonging to `types'.
   Stops reducing as soon as something else is encountered.
   `types' may be any combination of L_CONS_T, L_FILE_T, L_NAM_T, L_BOOL_T,
   L_CHAR_T, L_INT_T, L_FLT_T.
*/
#define red_type_n(t,n)	red_type_i_n(t, 1, n)
static Bool
red_type_i_n(int types, int i, int n)
{
  for (; n--; i++)
    if (!(L_TYPE(A(i)) & types) && !(L_TYPE(L_reduce_lazy(A(i))) & types))
      return 0;
  return 1;
}

static IntT Iabs(IntT a) { return a < 0 ? -a : a; }

/* Approximate square root ((sqrt n)^2 <= n). */
static IntT
Isqrt(IntT n)
{
  IntT a, b;

  if (n < 0) return 0;

  a = 0;
  b = n + 1;
  while (a + 1 != b) {
    IntT d = div2(a + b);

    /* Guard against too large squares! */
#ifdef LONGLONG_INTS
    if (d > 3037000499LL || sqr(d) > n)
#else
    if (d > 46340 || sqr(d) > n)
#endif
      b = d;
    else
      a = d;
  }
  return a;
}

/* Returns x to-the-power y; uses recursion based on halving of y. */
static IntT
Ipow(IntT x, IntT y)
{
  IntT z;

  if (y <= (IntT) 0 || x == (IntT) 1) return (IntT) 1;
  if (x == (IntT) 0) return (IntT) 0;
  z = Ipow(x, div2(y));
  z *= z;
  return (y & (IntT) 1) ? x * z : z;
}

#if 0
/* Returns length of list E.
   Much like L_reduce_seq but assumes upon entry that E is indeed a cons cell
   perhaps nil.
*/
static int
L_length(L_EXPR E)
{
  DCL_GCPRO1;
  int len = 0;

  /* Must protect the current cons cell: */
  GCPRO1(E);
  while (!L_NIL_P(E)) {
    E = L_CDR(E) = L_reduce_lazy(L_CDR(E));
    len++;
    if (!L_CONS_P(E)) break;
  }
  UNGCPRO();
  return len;
}
#endif

/* Recursion level during reduction(reduce_lazy): */
static int level = 0;

/* Prints next redex to be reduced. */
static void
print_red_entry(L_EXPR E)
{
  fprintf(stderr, "%2d  : ", level);
  L_print_trunc(stderr, E, 73);
  putc('\n', stderr);
}

/* Prints result of reducing redex printed by routine above. */
static void
print_red_exit(L_EXPR E)
{
  fprintf(stderr, "%2d => ", level);
  L_print_trunc(stderr, E, 73);
  putc('\n', stderr);
}

/* The reducer core.
   With respect to garbage collection, we here rely as much as possible on the
   fact that anything that's on the spine stack or can be reached from a cell
   on the spine stack will be safe, because upon entry the argument will be
   directly protected. Care must be taken not to violate the above property
   when overwriting the root of a redex. For instance, when assigning to
   L_RATOR(E) with E the root of a redex, the whole graph starting from the
   old L_RATOR(E) value becomes in principle unprotected. This is especially
   tricky when redex arguments A(1), A(2), et cetera, are APP2-ed.
   Also, don't forget that A(1) etc. are macros that refer to the stack
   relative to its top; so don't expect things to be ok when the top changes,
   e.g. by a drop_n or pop.
*/
static L_EXPR
L_reduce_lazy(L_EXPR E)
{
  int save_level = level++;
  unsigned int root = save_top();
  L_EXPR R = E;
  DCL_GCPRO1;

  if (level > MAX_REDUCE_LEVEL)
    exception("Maximum reduction depth exceeded.");

  /* Upon entry the argument E will immediately be protected(via R).
     Note that only application cells will be pushed on the spine stack;
     constants, built-ins, and combinators will never reside there.
     By the way, this is the only time that we protect something during
     reduction (except for PF_LEN). The dynamic gcpro_list therefore has
     roughly as many entries as there are outstanding recursive calls to
     this routine.
  */
  GCPRO1(R);

  do {

    if (interrupted)
      longjmp(Exception, 1);
      /* UNREACHABLE */

    switch (L_TYPE(E)) {

/* COMBINATORS: */

    case L_COMB_T:
      {
	int       comb = L_COMB(E);
	unsigned int k = COMB_ARITY(comb);
	unsigned int n = nr_pushed(root);

	if (n < k)
	  /* Too few arguments. */
	  goto break_loop;

	E = peek_down(k);
	/* E now points to root of redex(an application node). */

	/* Decided against tracing combinator reductions in verbose mode. */
	if (reduce_debug) print_red_entry(E);

/* Summary of combinator reduction rules:
   1a. Y f        --> f (Y f)
   1b. I x        --> x
   2.  K c x      --> c
   3a. S f g x    --> (f x) (g x)
   3b. B f g x    --> f (g x)
   3c. C f g x    --> (f x) g
   4a. S' c f g x --> c (f x) (g x)
   4b. B* c f g x --> c (f (g x))
   4c. C' c f g x --> c (f x) g
*/

	switch (comb) {
	case Y_COMB:
	  /* 1a. (Y f)             --> f (Y f) */
	  L_RATOR(E) = A(1);
	  /* This introduces cycles in the reduction graph: */
	  L_RAND (E) = E /*Y_APP(A(1))*/;
	  break;

	case I_COMB:
	  /* 1b. (I x)             --> x */
	  /* FALL THROUGH */
	case K_COMB:
	  {
	    L_EXPR A1 = A(1);

	    /* 2. ((K c) x)          --> c */
	    if (E == A1)
	      exception("Endless loop! Reduction aborted.");
	    L_OVERWRITE(E, A1);
	  }
	  break;

	case S_COMB:
	  {
	    L_EXPR A3 = A(3);

	    /* 3a.(((S f) g) x)      --> (f x) (g x) */
	    L_RAND (E) = APP2(A(2), A3); /* Order is vital! */
	    L_RATOR(E) = APP2(A(1), A3);
	  }
	  break;

	case B_COMB:
	  /* 3b.(((B f) g) x)      --> f (g x) */
	  L_RAND (E) = APP2(A(2), A(3)); /* Order is vital! */
	  L_RATOR(E) = A(1);
	  break;

	case C_COMB:
	  /* 3c.(((C f) g) x)      --> (f x) g */
	  L_RATOR(E) = APP2(A(1), A(3));
	  L_RAND (E) = A(2);
	  break;

	case Sp_COMB:
	  {
	    L_EXPR A2 = A(2);
	    L_EXPR A4 = A(4);

	    /* 4a.((((S' c) f) g) x) --> (c (f x)) (g x) */
	    /*Tricky! Order is vital! */
	    L_RAND (E) = APP2(A(3), A4);
	    L_RATOR(E) = APP2(A(1), A2);
	    L_RAND (L_RATOR(E)) = APP2(A2, A4);
	  }
	  break;

	case Bs_COMB:
	  /* 4b.((((B* c) f) g) x) --> c (f (g x)) */
	  L_RAND (E) = APP2(A(3), A(4)); /* Order is vital! */
	  L_RAND (E) = APP2(A(2), L_RAND(E));
	  L_RATOR(E) = A(1);
	  break;

	case Cp_COMB:
	  {
	    L_EXPR A3 = A(3);

	    /* 4c.((((C' c) f) g) x) --> (c (f x)) g */
	    /* Again tricky! */
	    L_RAND (E) = APP2(A(2), A(4));
	    L_RATOR(E) = APP2(A(1), L_RAND(E));
	    L_RAND (E) = A3;
	  }
	  break;
	} /*switch*/

	/* Here: redex root overwritten with combinator result.
	   Must pop the original combinator expression and proceed
	   with reduction of result E.
	*/
	drop_n(k);

	if (reduce_debug) {
	  /* Normally only counting reductions of built-ins. */
	  nr_reductions++; 
	  print_red_exit(E);
	}

	continue;
      }
      /* UNREACHABLE */

/* BUILT-IN FUNCTIONS: */

    case L_PF_T:
      {
	int       type = L_PF(E);
	unsigned int k = PF_ARITY(type);
	unsigned int n = nr_pushed(root);
	Bool ok = 0;

	/* Note: Built-in could be used as section (curried). */

	if (n < k)
	  /* Too few arguments. */
	  goto break_loop;

	if (k > 0)
	  E = peek_down(k); /* Watch out: *top is undefined. */
	  /* E is application cell. */
        /* else E is STDIN */

	if (reduce_verbose) print_red_entry(E);

	switch (type) {

	case PF_TYPE:
	  {
	    L_EXPR A1 = L_reduce_lazy(A(1));

	    ok = 1;
	    L_TYPE(E) = L_NAM_T;
	    switch (L_TYPE(A1)) {
	    case L_COMB_T:
	      L_NAME(E) = Canonise("Combinator");
	      break;
	    case L_APP_T:
	      L_NAME(E) = Canonise("Application");
	      break;
	    case L_PF_T:
	      L_NAME(E) = Canonise("Function");
	      break;
	    case L_CONS_T:
	      L_NAME(E) = Canonise("List");
	      break;
	    case L_FILE_T:
	      L_NAME(E) = Canonise("File");
	      break;
	    case L_NAM_T:
	      L_NAME(E) = Canonise("Name");
	      break;
	    case L_BOOL_T:
	      L_NAME(E) = Canonise("Bool");
	      break;
	    case L_CHAR_T:
	      L_NAME(E) = Canonise("Char");
	      break;
	    case L_INT_T:
	      L_NAME(E) = Canonise("Int");
	      break;
	    case L_FLT_T:
	      L_NAME(E) = Canonise("Float");
	      break;
	    }
	  }
	  break;

	case PF_NOT:
	  if (ok = red_type_n(L_BOOL_T, 1))
	    L_OVERWRITE(E, L_BOOL(A(1)) ? L_False : L_True);
	  break;

	case PF_POW:
	  if (ok = red_type_n(L_NUM_T, 2)) {
	    if (L_INT_P(A(1)) && L_INT_P(A(2)))
	      UPDATE_E_INT(Ipow(L_INT(A(1)), L_INT(A(2))));
	    else
	      UPDATE_E_FLT(pow(COERCE(A(1)), COERCE(A(2))));
	  }
	  break;

	case PF_SIN:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(sin(COERCE(A(1))));
	  break;
	case PF_COS:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(cos(COERCE(A(1))));
	  break;
	case PF_TAN:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(tan(COERCE(A(1))));
	  break;
	case PF_ASIN:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(asin(COERCE(A(1))));
	  break;
	case PF_ACOS:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(acos(COERCE(A(1))));
	  break;
	case PF_ATAN:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(atan(COERCE(A(1))));
	  break;
	case PF_SINH:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(sinh(COERCE(A(1))));
	  break;
	case PF_COSH:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(cosh(COERCE(A(1))));
	  break;
	case PF_TANH:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(tanh(COERCE(A(1))));
	  break;
	case PF_ASINH:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(asinh(COERCE(A(1))));
	  break;
	case PF_ACOSH:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(acosh(COERCE(A(1))));
	  break;
	case PF_ATANH:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(atanh(COERCE(A(1))));
	  break;

	case PF_EXP:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(exp(COERCE(A(1))));
	  break;
	case PF_LOG:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(log(COERCE(A(1))));
	  break;
	case PF_LOG10:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(log10(COERCE(A(1))));
	  break;
	case PF_LOG2:
	  if (ok = red_type_n(L_NUM_T, 1))
	    /* log2(x) = log(x)/log(2) */
	    UPDATE_E_FLT(log(COERCE(A(1))) / 0.69314718055994530942);
	  break;

	case PF_SQRT:
	  if (ok = red_type_n(L_NUM_T, 1)) {
	    if (L_INT_P(A(1)))
	      UPDATE_E_INT(Isqrt(L_INT(A(1))));
	    else
	      UPDATE_E_FLT( sqrt(L_FLT(A(1))));
	  }
	  break;
	case PF_ABS:
	  if (ok = red_type_n(L_NUM_T, 1)) {
	    if (L_INT_P(A(1)))
	      UPDATE_E_INT(Iabs(L_INT(A(1))));
	    else
	      UPDATE_E_FLT(fabs(L_FLT(A(1))));
	  }
	  break;
	case PF_CEIL:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(ceil(COERCE(A(1))));
	  break;
	case PF_FLOOR:
	  if (ok = red_type_n(L_NUM_T, 1))
	    UPDATE_E_FLT(floor(COERCE(A(1))));
	  break;

	case PF_TIMES:
	  if (ok = red_type_n(L_NUM_T, 2)) {
	    if (L_INT_P(A(1)) && L_INT_P(A(2)))
	      UPDATE_E_INT(L_INT(A(1)) * L_INT(A(2)));
	    else
	      UPDATE_E_FLT(COERCE(A(1)) * COERCE(A(2)));
	  }
	  break;

	case PF_DIVIDE:
	  if (ok = red_type_n(L_NUM_T, 2)) {
	    if (L_INT_P(A(1)) && L_INT_P(A(2))) {
	      if (L_INT(A(2)))
		UPDATE_E_INT(L_INT(A(1)) / L_INT(A(2)));
	      else
		exception("Attempt at integer divide by 0.");
	    }
	    else {
	      double A2 = COERCE(A(2));

	      if (A2)
		UPDATE_E_FLT(COERCE(A(1)) / A2);
	      else
		exception("Attempt at float divide by 0.");
	    }
	  }
	  break;

	case PF_MODULO:
	  if (ok = red_type_n(L_INT_T, 2)) {
	    if (L_INT(A(2)))
	      UPDATE_E_INT(L_INT(A(1)) % L_INT(A(2)));
	    else
	      exception("Attempt at modulo 0.");
	  }
	  break;

	case PF_PLUS:
	  if (ok = red_type_n(L_NUM_T|L_CHAR_T, 1)) {
	    if (L_FLT_P(A(1))) {
	      if (ok = red_type_i_n(L_NUM_T, 2, 1))
		UPDATE_E_FLT(L_FLT(A(1)) + COERCE(A(2)));
	    }
	    else
	    if (ok = red_type_i_n(L_NUM_T|L_CHAR_T, 2, 1)) {
	      if (L_FLT_P(A(2))) {
		if (L_INT_P(A(1)))
		  UPDATE_E_FLT(COERCE(A(1)) + L_FLT(A(2)));
	      }
	      else {
		L_TYPE(E) = L_TYPE(A(1));
		L_INT (E) = L_INT(A(1)) + L_INT(A(2));
	      }
	    }
	  }
	  break;

	case PF_MINUS:
	  if (ok = red_type_n(L_NUM_T|L_CHAR_T, 1)) {
	    if (L_FLT_P(A(1))) {
	      if (ok = red_type_i_n(L_NUM_T, 2, 1))
		UPDATE_E_FLT(L_FLT(A(1)) - COERCE(A(2)));
	    }
	    else
	    if (ok = red_type_i_n(L_NUM_T|L_CHAR_T, 2, 1)) {
	      if (L_FLT_P(A(2))) {
		if (L_INT_P(A(1)))
		  UPDATE_E_FLT(COERCE(A(1)) - L_FLT(A(2)));
	      }
	      else {
		L_TYPE(E) = L_TYPE(A(1));
		L_INT (E) = L_INT(A(1)) - L_INT(A(2));
	      }
	    }
	  }
	  break;

	case PF_SHIFTL:		/* Logic shift left! */
	  if (ok = red_type_n(L_INT_T, 2))
	    UPDATE_E_INT(((UIntT) L_INT(A(1))) << L_INT(A(2)) % BITSPERINT);
	  break;

	case PF_SHIFTR:
	  if (ok = red_type_n(L_INT_T, 2))
	    UPDATE_E_INT(((UIntT) L_INT(A(1))) >> L_INT(A(2)) % BITSPERINT);
	  break;

	case PF_LT:
	  if (ok = red_type_n(L_ORD_T, 2)) {
	    if (L_FLT(A(1)) || L_FLT(A(2)))
	      UPDATE_E_BOOL(COERCE(A(1)) <  COERCE(A(2)));
	    else
	      UPDATE_E_BOOL(L_INT(A(1)) <  L_INT(A(2)));
	  }
	  break;

	case PF_LE:
	  if (ok = red_type_n(L_ORD_T, 2)) {
	    if (L_FLT(A(1)) || L_FLT(A(2)))
	      UPDATE_E_BOOL(COERCE(A(1)) <= COERCE(A(2)));
	    else
	      UPDATE_E_BOOL(L_INT(A(1)) <= L_INT(A(2)));
	  }
	  break;

	case PF_GT:
	  if (ok = red_type_n(L_ORD_T, 2)) {
	    if (L_FLT(A(1)) || L_FLT(A(2)))
	      UPDATE_E_BOOL(COERCE(A(1)) >  COERCE(A(2)));
	    else
	      UPDATE_E_BOOL(L_INT(A(1)) >  L_INT(A(2)));
	  }
	  break;

	case PF_GE:
	  if (ok = red_type_n(L_ORD_T, 2)) {
	    if (L_FLT(A(1)) || L_FLT(A(2)))
	      UPDATE_E_BOOL(COERCE(A(1)) >= COERCE(A(2)));
	    else
	      UPDATE_E_BOOL(L_INT(A(1)) >= L_INT(A(2)));
	  }
	  break;

#if 0
/* Old semantics till Jan 4, 1996 (only numbers/chars/bools) */
	case PF_EQ:
	case PF_NE:
	  if (ok = red_type_n(L_NUM_T|L_CHAR_T|L_BOOL_T, 2)) {
	    switch (type) {
	    case PF_EQ:
	      if (L_FLT(A(1)) || L_FLT(A(2)))
		UPDATE_E_BOOL(COERCE(A(1)) == COERCE(A(2)));
	      else
		UPDATE_E_BOOL(L_INT(A(1)) == L_INT(A(2)));
	      break;
	    case PF_NE:
	      if (L_FLT(A(1)) || L_FLT(A(2)))
		UPDATE_E_BOOL(COERCE(A(1)) != COERCE(A(2)));
	      else
		UPDATE_E_BOOL(L_INT(A(1)) != L_INT(A(2)));
	      break;
	    } /*switch*/
	  }
	  else
	  if (ok = red_type_n(L_NAM_T, 2)) {
	    switch (type) {
	    case PF_EQ:
	      UPDATE_E_BOOL(L_NAME(A(1)) == L_NAME(A(2)));
	      break;
	    case PF_NE:
	      UPDATE_E_BOOL(L_NAME(A(1)) != L_NAME(A(2)));
	      break;
	    } /*switch*/
	  }
	  break;
#endif

	/* [In]equality on names/numbers/chars/bools/nil */
	case PF_EQ:
	case PF_NE:
	  {
	    L_EXPR A1 = L_reduce_lazy(A(1));
	    L_EXPR A2 = L_reduce_lazy(A(2));

	    ok = 1;
	    switch (L_TYPE(A1)) {
	    case L_NAM_T:
	      UPDATE_E_BOOL(L_NAM_P(A2) && L_NAME(A1) == L_NAME(A2));
	      break;
	    case L_BOOL_T:
	    case L_CHAR_T:
	    case L_INT_T:
	    case L_FLT_T:
	      UPDATE_E_BOOL((L_NUM_P(A2) || L_CHAR_P(A2) || L_BOOL_P(A2))
			     && COERCE(A1) == COERCE(A2));
	      break;

	    case L_CONS_T:
	      if (L_NIL_P(A1))
		UPDATE_E_BOOL(L_CONS_P(A2) && L_NIL_P(A2));
	      else
	      if (L_CONS_P(A2) && L_NIL_P(A2))
		L_OVERWRITE(E, L_False);
	      else {
		/* both non-Nil CONS cells. */
		L_EXPR A1 = A(1);
		L_EXPR A2 = A(2);
		L_EXPR F, R;

		F = APP3(mk_op_cell(type), L_CAR(A1), L_CAR(A2));
		R = APP3(mk_op_cell(type), L_CDR(A1), L_CDR(A2));
		L_RAND(E) = R;
		L_RAND(L_RATOR(E)) = F;
		R = L_RATOR(L_RATOR(E)) =
		  mk_op_cell(type == PF_NE ? PF_OR : PF_AND);
		/* Quick resume: */
		nr_reductions++;
		if (reduce_verbose) print_red_exit(E);
		E = R;
		continue;
	      }
	      break;

	    default:
	      if (    L_NAM_P (A2) || L_INT_P (A2)
		  ||  L_CHAR_P(A2) || L_BOOL_P(A2)
		  ||(L_CONS_P(A2) && L_NIL_P(A2)))
		L_OVERWRITE(E, L_False);
	      else
		ok = 0;
	      break;
	    } /*switch*/
	    if (ok && type == PF_NE)
	      L_BOOL(E) = !L_BOOL(E);
	  }
	  break;

	case PF_AND:
	  if (ok = red_type_n(L_BOOL_T, 1)) {
	    if (!L_BOOL(A(1)))
	      L_OVERWRITE(E, L_False);
	    else /* True */
	    if (ok = red_type_n(L_BOOL_T, 2))
	      UPDATE_E_BOOL(L_BOOL(A(2)));
	  }
	  break;

	case PF_OR:
	  if (ok = red_type_n(L_BOOL_T, 1)) {
	    if (L_BOOL(A(1)))
	      L_OVERWRITE(E, L_True);
	    else /* False */
	    if (ok = red_type_n(L_BOOL_T, 2))
	      UPDATE_E_BOOL(L_BOOL(A(2)));
	  }
	  break;

	case PF_IMPLIES:
	  if (ok = red_type_n(L_BOOL_T, 1)) {
	    if (!L_BOOL(A(1)))
	      L_OVERWRITE(E, L_True);
	    else /* True */
	    if (ok = red_type_n(L_BOOL_T, 2))
	      UPDATE_E_BOOL(L_BOOL(A(2)));
	  }
	  break;

	case PF_IF:
	  if (red_type_n(L_BOOL_T, 1)) {
	    L_EXPR A23 = L_BOOL(A(1)) ? A(2) : A(3);

	    drop_n(3);
	    FREE_E();
	    L_OVERWRITE(E, L_reduce_lazy(A23));
	    goto continue_loop;
	  }
	  break;

	case PF_CONS:
	  ok = 1;
	  L_TYPE(E) = L_CONS_T;
	  L_CAR (E) = A(1);
	  /* Note: L_RAND(E) == L_CDR(E) == A(2) */
	  break;

	/* Must do recursive reduction for all projection functions like
	   IF, CAR, CDR before updating the redex root E. This for the sake
	   of referential transparency.
	   (Also for APPEND's 2nd arg).
	   Note: first popping is possible and keeps the spine stack smaller.
	*/
	case PF_CAR:
	  if (red_type_n(L_CONS_T, 1)) {
	    L_EXPR car_A1 = L_CAR(A(1));

	    drop_n(1);
	    FREE_E();
	    L_OVERWRITE(E, L_reduce_lazy(car_A1));
	    goto continue_loop;
	  }
	  break;

	case PF_CDR:
	  if (red_type_n(L_CONS_T, 1)) {
	    L_EXPR cdr_A1 = L_CDR(A(1));

	    drop_n(1);
	    FREE_E();
	    L_OVERWRITE(E, L_reduce_lazy(cdr_A1));
	    goto continue_loop;
	  }
	  break;

	case PF_NULL:
	  if (ok = red_type_n(L_CONS_T, 1))
	    L_OVERWRITE(E, L_NIL_P(A(1)) ? L_True : L_False);
	  break;

	case PF_LEN:
	  if (ok = red_type_n(L_CONS_T, 1)) {
	    DCL_GCPRO1;
	    L_EXPR n, X, A1 = A(1);
	    IntT *len;

	    drop_n(1);

	    /* Using some round-about code here to ensure graceful
	       recovery from interrupts. Length of list is calculated
	       by keeping an accumulator cell `n' that will be added
	       to the length of the momentary list. Note that we allow
	       for garbage collection to reclaim any list cells of A1 that
	       are no longer used.
	    */
	    /* Duplicate(len A1) application: */
	    X = L_RAND(E) = APP2(L_RATOR(E), L_RAND(E));
	    /* Make accumulator (protect!) and keep pointer to its contents: */
	    L_RATOR(E) = n = mk_num_cell(0);
	    len = &L_INT(n);
	    /* Build (+ n) application: */
	    L_RATOR(E) = APP2(mk_op_cell(PF_PLUS), n);
	    /* Here: E = (+ n (len A1)), n = 0. */

	    /* Must only protect the current cons cell of A1: */
	    GCPRO1(A1);
	    while (!L_NIL_P(A1)) {
	      L_RAND(X) = L_CDR(A1);
	     (*len)++;
	      /* Here: E = (+ (n+1) (len (cdr A1))). */
	      A1 = L_CDR(A1) = L_reduce_lazy(L_CDR(A1));
	      if (!L_CONS_P(A1)) break;
	    }
	    UNGCPRO();
	    L_OVERWRITE(E, n);
	  }
	  break;

	case PF_ITH:
	  if (ok = red_type_n(L_INT_T|L_CHAR_T, 1)) {
	    IntT i = L_INT(A(1));

	    if (i >= 0) {
	      L_EXPR A2 = A(2);

	      drop_n(2);

	      do {
		if (L_CONS_P(A2) || L_CONS_P(L_reduce_lazy(A2))) {
		  if (L_NIL_P(A2))
		    break;

		  if (!i--) {
		    FREE_E();
		    L_OVERWRITE(E, L_reduce_lazy(L_CAR(A2)));
		    goto continue_loop;
		  }
		  A2 = L_CDR(A2);
		}
		else
		  break;
	      } forever;
	    }
	    L_OVERWRITE(E, L_Nil);
	  }
	  break;

	case PF_SEQ:
	  {
	    L_EXPR A1 = A(1);
	    L_EXPR A2 = A(2);

	    drop_n(2);
	    /* Result of 1st arg is simply discarded. */
	    L_reduce_seq(A1);
	    FREE_E();
	    L_OVERWRITE(E, L_reduce_lazy(A2));
	  }
	  goto continue_loop;

	case PF_FORCE:
	  {
	    L_EXPR A1 = A(1);

	    drop_n(1);
	    FREE_E();
	    L_OVERWRITE(E, L_reduce_force(A1));
	  }
	  goto continue_loop;

	case PF_APPEND:
	  if (ok = red_type_n(L_CONS_T, 1)) {
	    if (L_NIL_P(A(1))) { /* ++ [] A(2) => A(2) */
	      L_EXPR A2 = A(2);

	      drop_n(2);
	      FREE_E();
	      L_OVERWRITE(E, L_reduce_lazy(A2));
	      goto continue_loop;
	    }

	    /* Tricky! Order is vital. */
	    /* Use L_Nil as dummy contents: */
	    L_RAND(E) = APP2(L_Nil, A(2));
	    /* Replace the dummy L_Nil: */
	    L_RATOR(L_RAND(E)) = APP2(L_append, L_CDR(A(1)));
	    /* Apply cons directly, so instead of:
	       L_RATOR(E) = APP2(L_Cons, L_CAR(A(1)));
	       do:
	    */
	    L_TYPE(E) = L_CONS_T;
	    L_CAR (E) = L_CAR(A(1));
	    drop_n(2);
	  }
	  break;

	case PF_TIME:
	  {
	    clock_t start_t = clock();

	    drop_n(1);
	    /* Order is vital for gc protection. */
	    L_RAND (E) = L_reduce_force(L_RAND(E));
	    L_RAND (E) = mk_cons_cell(L_RAND(E), L_Nil);
	    L_RATOR(E) = mk_num_cell((clock() - start_t) / CLOCKS_PER_SEC);
	    L_TYPE (E) = L_CONS_T;
	    ok = 1;
	  }
	  break;

	case PF_READ:
	case PF_WRITE:
	  {
	    L_EXPR fname = L_reduce_force(A(1));

	    if (L_CONS_P(fname)) {
	      int len = L_stringp(fname);

	      if (len > 0) {
		char *fnames =
		  L_sprint_string(MA_MALLOC_BYTES(len + 1, char *),
				   fname, 0);
		FILE *fp;

		if (type == PF_READ) {
		  if (!(fp = fopen(fnames, "r"))) {
		    fprintf(stderr, "Cannot read file %s.\n", fnames);
		    MA_FREE_BYTES(fnames, len + 1);
		  }
		  else {
		    L_EXPR file = mk_file_cell(fp, Canonise(fnames));

		    MA_FREE_BYTES(fnames, len + 1);
		    L_RATOR(E) = mk_op_cell(PF_GETC);
		    L_RAND (E) = file;
		    drop_n(1);
		    /* Root redex E not in WHNF. */
		    goto continue_loop;
		  }
		}
		/* PF_WRITE */
		{
		  L_EXPR s = L_reduce_lazy(A(2));

		  if (L_CONS_P(s)) {
		    if (!(fp = fopen(fnames, "w"))) {
		      fprintf(stderr, "Cannot write file %s.\n", fnames);
		      MA_FREE_BYTES(fnames, len + 1);
		    }
		    else {
		      L_EXPR file = mk_file_cell(fp, Canonise(fnames));

		      MA_FREE_BYTES(fnames, len + 1);
		      /* Protect from gc: */
		      L_RATOR(E) = file;
		      L_RATOR(E) = APP2(mk_op_cell(PF_PUTC), file);
		      drop_n(2);
		      /* Root redex E not in WHNF. */
		      goto continue_loop;
		    }
		  }
		}
		MA_FREE_BYTES(fnames, len + 1);
	      }
	      else
		fputs("Illegal string for file name.\n", stderr);
	    }
	    else
	      fputs("Expecting string as file name.\n", stderr);
	    L_OVERWRITE(E, L_Nil);
	  }
	  break;

	case PF_SHOWS:
	  {
	    L_EXPR A1 = L_reduce_force(A(1));
	    L_EXPR A2 = A(2);
	    char buf[32];	/* Enough to hold bool, char, int rep. */

	    drop_n(2);
	    if (L_CONS_P(A1)) {
	      if (L_NIL_P(A1)) {
		FREE_E();
		L_OVERWRITE(E, L_reduce_lazy(A2));
		goto continue_loop;
	      }

	      if (L_stringp(A1)) {
		/* Cannot destructively modify cdr of A1; must make copy. */
		L_OVERWRITE(E, L_show_string(A1, A2));
		ok = 1;
	      }
	    }
	    else
	    if (L_show_atom(buf, A1)) {
	      /* mk_str builds new String; safe to append to it. */
	      L_OVERWRITE(E, mk_str(buf, A2));
	      ok = 1;
	    }
	  }
	  break;

#if 0
	/* Don't like how these guys behave.
	   STDOUT and STDERR fully evaluate their argument, whereas
	   READ, WRITE, and STDIN do so nicely lazily.
	   Doing "stdout stdin;" causes the whole contents of
	   stdin to be represented as a FUN string. Oh boy!
	*/
	case PF_STDOUT:
	case PF_STDERR:
	  {
	    L_EXPR A1 = A(1);

	    drop_n(1);
	    L_put_data_raw(type == PF_STDOUT ? stdout : stderr,
			    L_reduce_force(A1));
	    L_OVERWRITE(E, L_RATOR(E));
	  }
	  goto continue_loop;
#endif

	/* These are much better implementations:
	   (they do not accept a single char as argument; it must be a list)
	*/
	case PF_STDOUT:
	case PF_STDERR:
	  {
	    L_EXPR s = L_reduce_lazy(A(1));

	    if (ok = L_CONS_P(s)) {
	      L_EXPR file = mk_cell(L_FILE_T);

	      if (type == PF_STDOUT) {
		L_FILE_FP (file) = stdout;
		L_FILE_NAM(file) = Canonise("stdout");
	      }
	      else {
		L_FILE_FP (file) = stderr;
		L_FILE_NAM(file) = Canonise("stderr");
	      }
	      drop_n(1);
	      /* Protect from gc by temp. assigning to RATOR: */
	      L_RATOR(E) = file;
	      /* Now create final RATOR: */
	      L_RATOR(E) = APP2(mk_op_cell(PF_PUTSTD), file);
	      L_reduce_lazy(E);
	      L_OVERWRITE(E, mk_op_cell(type));
	      /* Root redex E not in WHNF. */
	      goto continue_loop;
	    }
	  }
	  break;

	case PF_STDIN:
	  {
	    FILE    *fp = fdopen(dup(fileno(stdin)), "r");
	    L_EXPR file = mk_file_cell(fp, Canonise("stdin"));

	    /* Note: PF_STDIN cell gets overwritten. */
	    L_TYPE (E) = L_APP_T;
	    L_RATOR(E) = mk_op_cell(PF_GETC);
	    L_RAND (E) = file;
	    /* Make a new PF_STDIN cell so we won't drag its whole contents
	       along.
	    */
	    file = mk_cell(L_PF_T);
	    L_PF(file) = PF_STDIN;
	    OperatorsTable[PF_STDIN] = file;
	    /* Root redex E not in WHNF. */
	    goto continue_loop;
	  }
	  /*UNREACHABLE*/
	  break;

	case PF_ERROR:
	  {
	    L_EXPR msg = L_reduce_force(A(1));

	    if (!L_CONS_P(msg))
	      break;

	    if (!L_NIL_P(msg)) {
	      if (!L_stringp(msg))
		break;

	      L_print_string_raw(stderr, msg);
	      putc('\n', stderr);
	    }
	    ok = 1;
	    longjmp(Exception, 1);
	  }
	  /* UNREACHABLE */
	  break;

	case PF_GETC:
	  /* Expects L_FILE_T argument; Reads a character from it,
	     creates the cons cell: [ char : getc <file> ].
	     Will be called at least once from READ.
	     So the first char is always read; it could be EOF and then
	     the file is closed and nil is returned.
	     Open files that have no longer any references will be closed
	     during garbage collection.
	  */
	  {
	    L_EXPR file = A(1);
	    FILE    *fp = L_FILE_FP(file);
	    int       c = getc(fp);

	    ok = 1;

	    if (c == EOF) {
	      fclose(fp);
	      L_FILE_FP(file) = NULL;
	      L_OVERWRITE(E, L_Nil);
	    }
	    else {
	      L_NIL_P(E) = 0;
	      L_TYPE (E) = L_CONS_T;
	      /* Order is vital for proper gc protect! */
	      /* Note: L_CAR(E) == PF_GETC cell */
	      L_CDR  (E) = APP2(L_CAR(E), file);
	      L_CAR  (E) = mk_char_cell(c);
	    }
	  }
	  break;

	case PF_PUTSTD:
	case PF_PUTC:
	  /* Expects a L_FILE_T as 1st argument and a cons cell as 2nd arg.
	     Writes a character to file pointer of 1st arg that is taken
	     from car of 2nd. Evaluates that arg fully and also evaluates
	     cdr lazily. Creates the new call: putc <file>(cdr 2nd arg).
	     Ends only when 2nd argument is exhausted and then closes the
	     file and returns nil.
	  */
	  {
	    L_EXPR file = A(1);
	    FILE    *fp = L_FILE_FP(file);

	    /* Pre: L_CONS_P(A(2)) */
	    if (L_NIL_P(A(2))) {
	      if (type == PF_PUTC) fclose(fp);
	      L_FILE_FP(file) = NULL;
	      L_OVERWRITE(E, L_Nil);
	      ok = 1;
	    }
	    else {
	      L_EXPR c = L_reduce_force(L_CAR(A(2)));

	      if (L_CHAR_P(c)) {
		L_EXPR cdr_A2 = L_CDR(A(2));

		putc(L_CHAR(c), fp);
		drop_n(2);
		L_RAND(E) = L_reduce_lazy(cdr_A2);
		/* Root redex E not in WHNF. */
		goto continue_loop;
	      }
	    }
	  }
	  break;
	} /*switch*/
      }
      /* Here: if !ok then arguments of a wrong type. */
      nr_reductions++;
      if (reduce_verbose) print_red_exit(E);
      goto break_loop;

/* APPLICATION: */

    case L_APP_T:
      /* Only place where a push on spine stack occurs. */
      push(E);
      E = L_RATOR(E);
      continue;

/* ANYTHING ELSE, I.E., NAM, BOOL, INT, CHAR, OR CONS: */

    default:
      goto break_loop;

    } /*switch*/
    /* UNREACHABLE */

  continue_loop: /* Loop continue point for non-combinator built-ins. */
    nr_reductions++;
    if (reduce_verbose) print_red_exit(E);
  } forever;

 break_loop: /* Single exit point of routine(except for longjmp). */
  /* Here redex is in WHNF = Weak Head Normal Form. */
  restore_to(root);
  UNGCPRO();
  level = save_level;
  return R;
}

/* First E is lazily reduced. If this results in a cons cell then both its
   car and cdr will be recursively reduced.
   E will be protected from gc.
*/
static L_EXPR
L_reduce_force(L_EXPR E)
{
  DCL_GCPRO1;
  L_EXPR T;

  E = L_reduce_lazy(E);
  GCPRO1(E);
  T = E;
  while (L_CONS_P(T) && !L_NIL_P(T)) {
        L_CAR(T) = L_reduce_force(L_CAR(T));
    T = L_CDR(T) = L_reduce_lazy (L_CDR(T));
  }
  UNGCPRO();
  return E;
}

#if 0
/* Does the same as L_reduce_force but prints while doing reductions.
   Note: strings will be printed as list of chars.
*/
static L_EXPR
L_reduce_force_top(FILE *fp, L_EXPR E, int *dummy)
{
  DCL_GCPRO1;
  L_EXPR T;

  E = L_reduce_lazy(E);
  GCPRO1(E);
  T = E;

  if (!L_print_atom(fp, T)) {
    if (L_CONS_P(T)) {
      /* L_CONS_P(T) && !L_NIL_P(T) */

      fputs("[ ", fp);

      do {
	L_SET_PFLAG(T);

   	    L_CAR(T) = L_reduce_force_top(fp, L_CAR(T), dummy);
	T = L_CDR(T) = L_reduce_lazy (L_CDR(T));
	if (L_PFLAGGED(T))
	  /* Circular list. */
	  fputs(", ...", fp);
	else
	if (L_CONS_P(T)) {
	  if (!L_NIL_P(T)) {
	    fputs(", ", fp);
	    continue;
	  }
	}
	else { /* `dotted list' */
	  fputs(" : ", fp);
	  L_print(fp, T);
	}
	break;
      } forever;

      fputs(" ]", fp);

      T = E;
      while (L_CONS_P(T) && !L_NIL_P(T)) {
	L_RESET_PFLAG(T);
	T = L_CDR(T);
      }
    }
    else
      L_print(fp, T);
  }
  UNGCPRO();
  return E;
}
#endif

/* Does the same as L_reduce_force but prints while doing reductions.
   This version looks at first car of a list and if a char will assume
   whole list to be a proper string. Infinite strings are also no problem.
*/
static L_EXPR
L_reduce_force_top(FILE *fp, L_EXPR E, int *a_string)
{
  E = L_reduce_lazy(E);

  if (*a_string == 2) {
    if (L_CHAR_P(E)) {
      putc('\"', fp); /* " just to help Emacs C-mode a bit */
      *a_string = 1;
    }
    else {
      fputs("[ ", fp);
      *a_string = 0;
    }
  }

  switch (L_TYPE(E)) {
  case L_PF_T:
    fputs(PF_STR(L_PF(E)), fp);
    break;
  case L_NAM_T:
    fputs(L_NAME(E), fp);
    break;
  case L_BOOL_T:
    fputs(L_BOOL(E) ? "True" : "False", fp);
    break;
  case L_CHAR_T:
    if (!*a_string)
      putc('\'', fp);
    fputs(string_char(L_CHAR(E), *a_string), fp);
    if (!*a_string)
      putc('\'', fp);
    break;
  case L_INT_T:
    fputs(l2toa(L_INT(E)), fp);
    break;
  case L_FLT_T:
    fputs(dtoa(L_FLT(E)), fp);
    break;
  case L_CONS_T:
    if (L_NIL_P(E))
      fputs("[]", fp);
    else {
      DCL_GCPRO1;
      L_EXPR T;
      int astring = 2;

      GCPRO1(E);
      T = E;

      do {
	L_SET_PFLAG(T);

   	    L_CAR(T) = L_reduce_force_top(fp, L_CAR(T), &astring);
	T = L_CDR(T) = L_reduce_lazy (L_CDR(T));
	if (L_PFLAGGED(T)) {
	  /* Circular list. */
	  if (astring)
	    fputs("...", fp);
	  else
	    fputs(", ...", fp);
	  /*break*/
	}
	else
	if (L_CONS_P(T)) {
	  if (!L_NIL_P(T)) {
	    if (!astring) fputs(", ", fp);
	    continue;
	  }
	  /*break*/
	}
	else { /* `dotted list' */
	  fputs(" : ", fp);
	  L_print(fp, T);
	}
	break;
      } forever;

      if (astring)
	putc('\"', fp); /* " just to help Emacs C-mode a bit */
      else
	fputs(" ]", fp);

      T = E;
      while (L_CONS_P(T) && !L_NIL_P(T) && L_PFLAGGED(T)) {
	L_RESET_PFLAG(T);
	T = L_CDR(T);
      }
      UNGCPRO();
    }
    break;
  case L_FILE_T:
    fprintf(fp, "<file: %s>", L_FILE_NAM(E));
    break;
  case L_COMB_T:		/* can't have L_ABS_T here */
    assert(L_COMB(E));
    fputs(COMB_STR(L_COMB(E)), fp);
    break;
  case L_vE_T: /* NOT POSSIBLE */
  case L_APP_T:
    /* Only when not enough arguments. */
    L_print(fp, E);
  } /*switch*/
  fflush(fp);
  return E;
}

/* First E is lazily reduced. If this results in a cons cell then all of
   its cdr-chain will be reduced. Does NOT protect the whole list from gc,
   but merely the cons cell under reduction, i.e., any cons cells lying ahead
   might get garbage-protected in the meantime.
   Returns the number of cons cells encountered.
*/
static Nat
L_reduce_seq(L_EXPR E)
{
  DCL_GCPRO1;
  Nat len = 0;

  E = L_reduce_lazy(E);
  GCPRO1(E);
  while (L_CONS_P(E) && !L_NIL_P(E)) {
    E = L_CDR(E) = L_reduce_lazy(L_CDR(E));
    len++;
  }
  UNGCPRO();
  return len;
}

/* Documentation in reduce.h */
L_EXPR
L_reduce(FILE *fp, L_EXPR E)
{
  Bool top_print = 0;
  struct gcpro *save_gcprolist = gcprolist;

  /* Administer #reductions and stack ceiling per top-level reduction: */
  nr_reductions = 0;
  stack_ceiling = 0;

  /* This kludge avoids annoying repetition in printing first redex. */
  if (reduce_verbose) {
    if (L_APP_P(E)) {
      L_EXPR T = E;

      /* Get outer most-left rator: */
      do
	T = L_RATOR(T);
      while (L_APP_P(T));
      if (!L_PF_P(T) && !L_COMB_P(T)) top_print = 1;
    }
    else
      top_print = 1;
  }

  interrupted = 0;

  if (!setjmp(Exception)) {
    int astring = 0;

    signal(SIGINT, sigint_handler);

    if (fp && !reduce_verbose) {
      E = L_reduce_force_top(fp, E, &astring);
      fputc('\n', fp);
    }
    else {
      if (top_print) print_red_entry(E); else level--;
      E = L_reduce_force(E);
      if (top_print) print_red_exit(E); else level++;
    }
    signal(SIGINT, SIG_DFL);
  }
  else {
    restore_to(0);
    gcprolist = save_gcprolist;
    level = 0;
    if (interrupted) {
      /* The print flag might still be present. */
      while (L_CONS_P(E) && !L_NIL_P(E) && L_PFLAGGED(E)) {
	L_RESET_PFLAG(E);
	E = L_CDR(E);
      }
      if (fp) fflush(fp);
      fputs("Interrupted!\n", stderr);
      interrupted = 0;
    }
    E = L_Nil;
  }
  return E;
}
