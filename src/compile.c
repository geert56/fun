/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : compile.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1997 G.L.J.M. Janssen
 date	   :  8-DEC-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include "cell.h"
#include "compile.h"

/* ------------------------------------------------------------------------ */
/* IMPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

/* Define this to ensure that global definitions (functions and variables)
   are copied before they are reduced.
   Gofer does not use copying!
*/
/*#define COPY_DEFS*/

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */

static LIST marks_to_be_reset;

/* ------------------------------------------------------------------------ */
/* EXPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

Bool compile_verbose = 0;
Bool compile_optimise = 1;

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* SK COMPILER & OPTIMIZER                                                  */
/* ------------------------------------------------------------------------ */

/* Tries to optimise S E1 E2.
   Optimisations performed are (Curry/Turner):
   1. Opt [[ S (K p) (K q)   ]] = K (p q)
   2. Opt [[ S (K p) I       ]] = p
   3. Opt [[ S (K p) (B q r) ]] = B* p q r
   4. Opt [[ S (K p) q       ]] = B p q
   5. Opt [[ S (B p q) (K r) ]] = C' p q r
   6. Opt [[ S p (K q)       ]] = C p q
   7. Opt [[ S (B p q) r     ]] = S' p q r
   If none apply or global `compile_optimise' is false, result is S E1 E2.
*/
static L_EXPR
L_optimise(L_EXPR E1, L_EXPR E2)
{
  if (compile_optimise) {

  /* S E1 E2 */
  if (L_APP_P(E1)) { 			/* S (E3 E4) E2 */
    L_EXPR E3 = L_RATOR(E1);
    L_EXPR E4 = L_RAND (E1);

    if (L_K_P(E3)) {			/* S (K E4) E2 */
      if (L_I_P(E2))			/* S (K E4) I => E4 */
/*2*/	return E4;

      /* S (K E4) E2 */
      if (L_APP_P(E2)) {		/* S (K E4) (E5 E6) */
	L_EXPR E5 = L_RATOR(E2);
	L_EXPR E6 = L_RAND (E2);

	if (L_K_P(E5))			/* S (K E4) (K E6) => K (E4 E6) */
/*1*/	  return K_APP(APP2(E4, E6));

	/* S (K E4) (E5 E6) */
	if (L_APP_P(E5) && L_B_P(L_RATOR(E5)))
	  /* S (K E4) ((B E7) E6) => B* E4 E7 E6 */
/*3*/	  return Bs_APP(E4, L_RAND(E5), E6);

	/* S (K E4) (E5) => B E4 E2 */
      }
      /* S (K E4) E2 => B E4 E2 */
/*4*/ return B_APP(E4, E2);
    }

    /* S (E3 E4) E2 */
    if (L_APP_P(E3) && L_B_P(L_RATOR(E3))) {	/* S ((B E5) E4) E2 */
      if (L_APP_P(E2))		  /* S ((B E5) E4) (E6 E7) */
	if (L_K_P(L_RATOR(E2))) /* S ((B E5) E4) (K E7) => C' E5 E4 E7 */
/*5*/	  return Cp_APP(L_RAND(E3), E4, L_RAND(E2));
	/* S ((B E5) E4) (E6 E7) => S' E5 E4 E2 */

      /* S ((B E5) E4) E2 => S' E5 E4 E2 */
/*7*/ return Sp_APP(L_RAND(E3), E4, E2);
    }
    /* S (E3 E4) E2 */
  }

  /* S E1 E2 */
  if (L_APP_P(E2) && L_K_P(L_RATOR(E2)))	/* S E1 (K E4) => C E1 E4 */
/*6*/ return C_APP(E1, L_RAND(E2));
  }
  return S_APP(E1, E2);
}

/* Arg `B' already compiled: it's a combinatory expression, i.e.,
   applications of primitive combinators and possibly atoms.

   A v [[ v       ]] = I
   A v [[ x       ]] = K x
   A v [[ (E1 E2) ]] = Opt [[ S (A v [[ E1 ]]) (A v [[ E2 ]]) ]]
*/
static L_EXPR
L_compile_abs(const char *v, L_EXPR B)
{
  if (L_CFLAGGED(B))
    /* A marked node means that it is the root of a global definition.
       Any free variables in it cannot be bound, hence treat global
       definition as a constant.
       Must of course remove the flag to not clutter other uses of the
       mark bit. Unfortunately, cannot do so here because multiple
       references to the same definition may exist.
       Already put these nodes on the `marks_to_be_reset' list.
    */
    return K_APP(B);

  switch (L_TYPE(B)) {

  case L_APP_T:
    /* Possible speed-up: v not `used' in B => K B */
    return L_optimise(L_compile_abs(v, L_RATOR(B)),
		      L_compile_abs(v, L_RAND (B)));
  case L_NAM_T:
    if (v == L_NAME(B))
      /* L v . v => I */
      return I_APP();
    /* FALL THROUGH */
  default:
    /* L v . c => K c */
    return K_APP(B);
  } /*switch*/
  /* NOT REACHED */
}

/* Compiles lambda-expression `E' into SK combinators.
   Returns either atom, or an application of compiled rator and rand,
   or anything that L_compile_abs returns:

   C[[ Atom    ]] = Atom                  , C stands for L_compile_1
   C[[ (E1 E2) ]] = (C[[ E1 ]] C[[ E2 ]])
   C[[ L v . B ]] = CA v [[ C[[ B ]] ]]   , CA stands for L_compile_abs

   Must watch out for global let and letrec definitions.
   It should be possible to locally rebind those and also we should avoid
   doing recompilations. Any free variables occurring in global definitions
   cannot be bound, and thus remain free forever.
*/
static L_EXPR
L_compile_1(L_EXPR E)
{
  if (L_CFLAGGED(E))
    /* A marked node hints at an already compiled expression. */
    return E;

  switch (L_TYPE(E)) {

  case L_APP_T:
    /* Only need to recurse in presence of abstractions. */
    L_RATOR(E) = L_compile_1(L_RATOR(E));
    L_RAND (E) = L_compile_1(L_RAND (E));
    return E;

  case L_ABS_T:
    if (L_COMB(E)) return E;
    {
      const char *v = L_BV(E);
      L_EXPR      B = L_BODY(E);
      L_EXPR   name = get_name(v); /* watch out: name perhaps NULL because
				      it was never used in any expression.
				   */

      /* Could keep a stack of all abstraction variables.
	 (in fact, used exactly that approach initially).
	 It is cheaper to use a counter in the name cell instead.
	 Of course, assuming that all counters are 0 upon start of compilation.
	 L_NAME_MARK(name) > 0 means: there is an enclosing abstraction
	 for this name.
      */
      if (name) L_NAME_MARK(name)++;
      B = L_compile_1(B);
      if (name) L_NAME_MARK(name)--;
      return L_compile_abs(v, B);
    }

  case L_NAM_T:
    /* Check whether there is an outstanding binding (abstraction) for
       this variable. If so, may not attempt to interpret the variable as
       the name of a global definition.

       All (canonised) strings are uniquely stored in the NamesTable.
       Any use of such a string as an identifier will make sure that a
       unique L_NAM_T cell is associated with the string (this is done
       by mk_name_cell). However, a single mark bit will not work in case
       of nested bindings of the same identifier: mark bits cannot be stacked,
       but the same variable may well have multiple bindings. A solution is to
       keep a counter for each bound variable.
       This is now done for each name cell.
    */
    if (!L_NAME_MARK(E)) {
      /* name is free; perhaps there is a global definition for it.
	 Global definitions are already compiled.
      */
      L_EXPR X = E;

      /* Perhaps it is better to make a copy of the definition (but only
	 once for each top-level compile call).
	 That way globally defined names have a guaranteed non-circular
	 compiled definition. Not making a copy might cause the definition
	 to become a possibly cyclic graph during reduction and, what's
	 worse, some large data structure might be build and contained
	 in the graph, without the garbage collector being able to
	 reclaim it unless the definition is undone via "undef".
	 COPY_DEFS controls the choice of behaviour sketched above.
	 For uniformity will use EXTRA fields in both versions, although
	 this is not necessary for the non-copying one.
      */

      /* We keep the (marked) [single copy] of def. on the name EXTRA field. */
      if (E = L_NAME_EXTRA(X))
	return E;

      /* No def found on EXTRA field; see if we need to make one: */
      if (E = get_def(L_NAME(X))) {
	/* E is a global definition not previously seen during this
	   compilation run. [Copy it and] mark it to avoid recompilation
	   and remember to unmark it by putting the name cell on a list.
	*/
#ifdef COPY_DEFS
	E = L_cp(E);
#endif
	L_NAME_EXTRA(X) = E;
	L_SET_CFLAG(E);
	marks_to_be_reset = push_cont((void *) X, marks_to_be_reset);
      }
      else E = X;
    }
    /* FALL THROUGH */
  default:
    /* Treated as Atom. */
    return E;
  } /*switch*/
  /* NOT REACHED */
}

/* Documentation in compile.h */
L_EXPR
L_compile(L_EXPR E)
{
  marks_to_be_reset = NULL_LIST;
  E = L_compile_1(E);
  /* Only name cells are on the list. */
  while (marks_to_be_reset) {
    L_EXPR N = (L_EXPR) pop_cont(&marks_to_be_reset);

    L_RESET_CFLAG((L_EXPR) L_NAME_EXTRA(N));
    L_NAME_EXTRA(N) = NULL;
  }
  return E;
}
