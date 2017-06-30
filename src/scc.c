/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : scc.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1998 G.L.J.M. Janssen
 date	   :  8-DEC-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include "cell.h"

/* ------------------------------------------------------------------------ */
/* IMPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

/* Access macros to various fields of a VERTEX struct: */
#define V_LOWLINK(v)		((v)->lowlink)
#define V_SUCCS(v)		((v)->succs)
#define V_NAME(v)		((v)->name)
#define V_NEXT(v)		((v)->next)

/* Note: non-NULL empty-stack value so test STACKED can use
   `next' field and not get confused. */
#define EMPTY_STACK		((VERTEX) 1)
#define STACKED(v)		V_NEXT(v)

/* Push vertex on stack. Vertex `s' must NOT be NULL. */
#define push_stack(s)		( V_NEXT(s) = stack, stack = (s) )

/* Pops vertex from stack. Resets "on stack" condition.
   stack must NOT be empty.
*/
#define pop_stack() \
	( \
	 temp_stack = stack, \
	 stack = V_NEXT(stack), \
         STACKED(temp_stack) = NULL, \
	 temp_stack \
	)

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

typedef struct VERTEX *VERTEX;
struct VERTEX {
  unsigned int lowlink;		/* Tarjan's low link value */
  L_EXPR name;			/* Attribute of vertex */
  VERTEX next;			/* Next vertex on stack */
  VERTEX *succs;		/* Direct successors */
};

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */

/* Stack collecting vertices belonging to the same SCC during Tarjan Alg. */
static VERTEX stack;
static VERTEX temp_stack;	/* used by pop_stack */

/* Counting vertices during dfs exploration. */
static Nat dfs_number;

/* Actual number of vertices: */
static Nat NR_VERTICES;
/* Flag indicating whether SCCs should appear in reversed order: */
static Bool REVERSE_RESULT;

/* Components are identified with mutual dependent vi=Bi definitions.
   These are collected as a list per component and these themselves are
   collected in the list DD as final result.
*/
static LIST DD;

/* ------------------------------------------------------------------------ */
/* EXPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* IMPORTED FUNCTIONS                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* FUNCTION DEFINITIONS                                                     */
/* ------------------------------------------------------------------------ */

/* A variable is called free in E if it has at least 1 free occurrence in E.
   Marks all free variables in E by setting their L_NAME_MARK field
   in the name cell to some non-0 value, in fact sets it to 1.
   Returns number of distinct free variables (not occurrences).
*/
static Nat
L_mark_free_vars(L_EXPR E)
{
  Nat nr_free_vars = 0;

 restart:
  switch (L_TYPE(E)) {
  case L_ABS_T: /* Abstractions only present before compilation! */

    /* Combinators cannot be used as variables! */
    if (L_COMB(E))
      break;
    {
      L_EXPR name = get_name(L_BV(E));

      /* Same trick as in SK compiler.
	 L_NAME_MARK(name) > 0 means: there is an enclosing abstraction
	 for this name.
      */
      if (name) L_NAME_MARK(name)++;
      nr_free_vars += L_mark_free_vars(L_BODY(E));
      if (name) L_NAME_MARK(name)--;
    }
    break;

  case L_CONS_T:
    if (L_NIL_P(E))
      break;
    /* FALL THROUGH */
  case L_APP_T:
    nr_free_vars += L_mark_free_vars(L_RATOR(E));
    /* Avoiding tail-recursion: */
    E = L_RAND(E);
    goto restart;

  case L_vE_T: /* No longer present after parsing! */
    {
      L_EXPR name = get_name(L_vE_v(E));

      if (name && !L_NAME_MARK(name)) {
	/* There is no outstanding binding for this variable:
	   it's a free occurrence!
	*/
	L_NAME_MARK(name)++;
	nr_free_vars++;
      }
    }
    E = L_vE_E(E);
    goto restart;

  case L_NAM_T:
    if (!L_NAME_MARK(E)) {
      /* There is no outstanding binding for this variable:
	 it's a free occurrence!
      */
      L_NAME_MARK(E)++;
      nr_free_vars++;
    }
    break;

  default:
    break;
  } /*switch*/ 
  return nr_free_vars;
}

/* Reset all free variable markings. */
static void
L_unmark_free_vars(L_EXPR E)
{
 restart:
  switch (L_TYPE(E)) {
  case L_ABS_T:
    /* Combinators cannot be used as variables! */
    if (L_COMB(E))
      break;
    E = L_BODY(E);
    goto restart;

  case L_CONS_T:
    if (L_NIL_P(E))
      break;
    /* FALL THROUGH */
  case L_APP_T:
    L_unmark_free_vars(L_RATOR(E));
    E = L_RAND(E);
    goto restart;

  case L_vE_T:
    {
      L_EXPR name = get_name(L_vE_v(E));

      if (name) L_NAME_MARK(name) = 0;
    }
    E = L_vE_E(E);
    goto restart;

  case L_NAM_T:
    L_NAME_MARK(E) = 0;
    break;

  default:
    break;
  } /*switch*/ 
}

/* Modified Tarjan's strongly connected components algorithm based on
   depth-first search. As side-effect constructs return value of
   depend_analysis.
   Components are identified with mutual dependent vi=Bi definitions.
   These are collected as a list per component and these themselves are
   collected in the list DD as final result.

   [ Same algorithm was independently used by Mark P. Jones in Gofer. ]
*/
static void
dfs(VERTEX vertex)
{
  /* Initialise scc info (also marks vertex): */
  unsigned int lowlink_orig = V_LOWLINK(vertex) = ++dfs_number;
  Bool has_self_loop = 0;
  Nat i;

  /* Keep all vertices forming current SCC on a stack: */
  push_stack(vertex);

  /* Now examine each successor: */
  for (i = 0; i < NR_VERTICES; i++) {
    VERTEX succ = V_SUCCS(vertex)[i];

    /* Might have `holes' in successor array: */
    if (!succ) continue;

    if (!V_LOWLINK(succ))
      dfs(succ);

    if (STACKED(succ)) {
      if (V_LOWLINK(succ) < V_LOWLINK(vertex))
	V_LOWLINK(vertex) = V_LOWLINK(succ);
      has_self_loop |= vertex == succ;
    }
  }

  /* Here: dealt with all descendants of vertex */

  if (V_LOWLINK(vertex) == lowlink_orig) {
    LIST Ds = NULL_LIST;	/* per SCC sub-list result */
    VERTEX s;

    /* vertex is root of strongly connected component. */
    do {
      const char *vi;
      L_EXPR name, Bi;

      s = pop_stack();

      name = V_NAME(s);
      vi = L_NAME(name);
      Bi = L_NAME_EXTRA(name);
      /* Reset extra field in name: */
      L_NAME_EXTRA(name) = NULL;
      Ds = push_cont(mk_vE_cell(vi, Bi), Ds);
    } while (s != vertex);
    LIST_INFO(Ds) = has_self_loop;
    DD = REVERSE_RESULT ? append_cont(Ds, DD) : push_cont(Ds, DD);
  }
}

/* For documentation see scc.h */
LIST
depend_analysis(Nat n, LIST vs, LIST Bs, Bool rev_result)
{
  Nat i/*, j*/;
  /* Allocate n vertices: */
  VERTEX  nodes = CALLOC_ARRAY(n,     struct VERTEX);
  /* Allocate worst-case n^2 successor pointers: */
  VERTEX *succs = CALLOC_ARRAY(n * n,        VERTEX);
  VERTEX  p;
  VERTEX *q;

  /* Hand-out successor array of size n to each vertex: */
  for (p = nodes, q = succs, i = 0; i < n; i++, p++, q += n)
    V_SUCCS(p) = q;

  /* Fill name field of vertices: */
  /* Note: p = nodes + n */
  FOR_EACH_LIST_ELEM(vs, elem) {
    const char *vi = (const char *) ELEM_CONTENTS(elem);
    L_EXPR   namei = mk_name_cell(vi);

    p--;
    V_NAME(p) = namei;
  } END_FOR_EACH_LIST_ELEM;

  /* Determine dependencies and create directed graph, O(n^2): */
  /*j = n;*/
  p = nodes + n;
  while (Bs) {
    L_EXPR      Bj = (L_EXPR) pop_cont(&Bs);
    /*const char *vj;*/
    L_EXPR   namej;

    /*j--;*/ p--;
    namej = V_NAME(p);
    /*vj = L_NAME(namej);*/
    /* Temp save Bj in vj's name cell: */
    L_NAME_EXTRA(namej) = Bj;

    /* Mark the free variables in Bj: */
    L_mark_free_vars(Bj);

    /* Check whether any of the vi's are marked free as well: */
    i = n;
    FOR_EACH_LIST_ELEM(vs, elem) {
      const char *vi = (const char *) ELEM_CONTENTS(elem);
      L_EXPR   namei = get_name(vi);

      i--;
      if (L_NAME_MARK(namei))
	/* vi occurs free in Bj ==> vj depends on vi: vj = Bj(vi) */
	/* Add the edge(vj --> vi) to the graph: */
	V_SUCCS(p)[i] = nodes + i;

    } END_FOR_EACH_LIST_ELEM;
    L_unmark_free_vars(Bj);
  }
  /* Here: all Bj name cells unmarked, i = j = 0; also Bs list destroyed. */
  /* Free vs list: */
  free_list(vs, 0);

  /* Init for dfs scc calculation: */
  stack = EMPTY_STACK;
  dfs_number = 0;
  NR_VERTICES = n;
  DD = NULL_LIST;
  REVERSE_RESULT = rev_result;

  /* The scc calculation: */
  for (p = nodes, i = 0; i < n; i++, p++)
    if (!V_LOWLINK(p))
      dfs(p);

  /* Cleanup: */
  MA_FREE_ARRAY(succs, n*n,        VERTEX);
  MA_FREE_ARRAY(nodes,   n, struct VERTEX);

  return DD;
}
