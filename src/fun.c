/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : fun.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1999 G.L.J.M. Janssen
 date	   :  4-FEB-1999
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include "fun.h"
#include "print.h"
#include "compile.h"
#include "reduce.h"
#include "scc.h"

/* ------------------------------------------------------------------------ */
/* IMPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* Defined in lex.l: */
extern FILE *yyin;

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

/* Standard definitions filename.
   May be redefined for instance in Makefile via -D directive to $(CC).
   May again be redefined by FUN_STDDEFS shell environment variable.
*/
#ifndef FUN_STDDEFS
#define FUN_STDDEFS			"/usr/local/lib/stddefs.fun"
#endif

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */

static Bool FUN_PACKAGE_INITIALIZED = 0;

/* ------------------------------------------------------------------------ */
/* EXPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

Bool fun_verbose0 = 0;
Bool fun_verbose1 = 0;
Bool fun_debug    = 0;
Bool fun_echo     = 0;
Bool fun_virginal = 0;

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* IMPORTED FUNCTIONS                                                       */
/* ------------------------------------------------------------------------ */

/* Defined in lex.l: */
extern int parse_file (const char *name, FILE *fp);

/* Defined in y.y: */
extern void warning (char *format, ...);

/* ------------------------------------------------------------------------ */
/* FUNCTION DEFINITIONS                                                     */
/* ------------------------------------------------------------------------ */

/* Generates a unique variable name that is guaranteed not to clash with any
   of the user's names.
*/
static Nat unique_var_count = 0;

static const char *
unique_var(void)
{
  static char buf[16];

  sprintf(buf, "*%d", unique_var_count++);
  return Canonise(buf);
}

/* Returns 1 when variable `v' occurs free in expression `E';
   otherwise returns 0.
   Only makes sense to call after parsing and before compilation!
*/
static int
L_occurs_free(const char *v, L_EXPR E)
{
 restart:
  switch (L_TYPE(E)) {
  case L_ABS_T:
    /* Combinators cannot be used as variables! */
    if (L_COMB(E) || v == L_BV(E))
      return 0;
    E = L_BODY(E);
    goto restart;

  case L_CONS_T:
    /* Only present during and after reduction! */
    if (L_NIL_P(E))
      return 0;
    /* FALL THROUGH */
  case L_APP_T:
    if (L_occurs_free(v, L_RATOR(E)))
      return 1;
    E = L_RAND(E);
    goto restart;

  case L_vE_T:
    /* No longer present after parsing! */
    if (v == L_vE_v(E))
      return 1;
    E = L_vE_E(E);
    goto restart;

  case L_NAM_T:
    return v == L_NAME(E);

  case L_PF_T:
    /* Built-ins cannot be used as variables! */
  case L_BOOL_T:
  case L_CHAR_T:
  case L_INT_T:
  case L_FLT_T:
  case L_FILE_T:
    /* Only present during and after reduction! */
    return 0;
  } /*switch*/ 
  /* UNREACHABLE */
}

/* Interesting stuff, but of no use yet. */
#if 0

/* Returns the free variables in `E' as a list.
   A variable is called free in expression `E' if it has at least 1 free
   occurrence in `E'.
   The free variables in `E' are added at the end of the argument list
   `fvars' (usually the NULL_LIST).
*/
static LIST
L_free_vars(L_EXPR E, LIST fvars)
{
 restart:
  switch (L_TYPE(E)) {
  case L_ABS_T:
    /* Combinators cannot be used as variables! */
    if (L_COMB(E)) return fvars;
    {
      L_EXPR name = get_name(L_BV(E));

      /* Same trick as in compiler.
	 L_NAME_MARK(name) > 0 means: there is an enclosing abstraction
	 for this name.
      */
      if (name) L_NAME_MARK(name)++;
      fvars = L_free_vars(L_BODY(E), fvars);
      if (name) L_NAME_MARK(name)--;
      return fvars;
    }

  case L_CONS_T:
    /* Only present during and after reduction! */
    if (L_NIL_P(E))
      return fvars;
    /* FALL THROUGH */
  case L_APP_T:
    fvars = L_free_vars(L_RATOR(E), fvars);
    E = L_RAND(E);
    goto restart;

  case L_vE_T:
    /* No longer present after parsing! */
    {
      L_EXPR name = get_name(L_vE_v(E));

      if (name && !L_NAME_MARK(name))
	fvars = append_cont(L_vE_v(E), fvars);
    }
    E = L_vE_E(E);
    goto restart;

  case L_NAM_T:
    if (!L_NAME_MARK(E))
      return append_cont(L_NAME(E), fvars);
    /* FALL THROUGH */
  default:
    return fvars;
  } /*switch*/ 
}

static void print_var(FILE *fp, const char *v) { fputs(v, fp); }

static void
print_free_vars(L_EXPR E)
{
  print_list(stderr, "Free vars: ", L_free_vars(E, NULL_LIST),
	     (void (*)(FILE *, void *)) print_var, ", ", ".\n");
}

/* Get a list of all occurrences of variables in `E'.
   In fact appends these vars to list `occurs'.
   Excludes the occurrences as an abstraction variable.
*/
static LIST
L_occurs_vars(L_EXPR E, LIST occurs)
{
 restart:
  switch (L_TYPE(E)) {
  case L_ABS_T:
    /* Combinators cannot be used as variables! */
    if (L_COMB(E)) return occurs;
    E = L_BODY(E);
    goto restart;

  case L_CONS_T:
    /* Only present during and after reduction! */
    if (L_NIL_P(E))
      return occurs;
    /* FALL THROUGH */
  case L_APP_T:
    occurs = L_occurs_vars(L_RATOR(E), occurs);
    E = L_RAND(E);
    goto restart;

  case L_vE_T:
    /* No longer present after parsing! */
    occurs = append_cont(L_vE_v(E), occurs);
    E = L_vE_E(E);
    goto restart;

  case L_NAM_T:
    return append_cont(L_NAME(E), occurs);

  default:
    return occurs;
  } /*switch*/ 
}

/* Returns 1 when variable `v' occurs bound in expression `E';
   otherwise returns 0.
   Only makes sense to call after parsing and before compilation!
*/
static int
L_occurs_bound(const char *v, L_EXPR E)
{
 restart:
  switch (L_TYPE(E)) {
  case L_ABS_T:
    /* Combinators cannot be used as variables! */
    if (L_COMB(E))
      return 0;
    if (v == L_BV(E))
      return L_occurs_free(v, L_BODY(E));
    E = L_BODY(E);
    goto restart;

  case L_CONS_T:
    /* Only present during and after reduction! */
    if (L_NIL_P(E))
      return 0;
    /* FALL THROUGH */
  case L_APP_T:
    if (L_occurs_bound(v, L_RATOR(E)))
      return 1;
    E = L_RAND(E);
    goto restart;

  case L_vE_T:
    /* No longer present after parsing! */
    E = L_vE_E(E);
    goto restart;

  case L_NAM_T:
  case L_PF_T:
  case L_BOOL_T:
  case L_CHAR_T:
  case L_INT_T:
  case L_FLT_T:
  case L_FILE_T:
    /* Only present during and after reduction! */
    return 0;
  } /*switch*/ 
}
#endif

/* ------------------------------------------------------------------------ */
/* CONSTRUCTOR FUNCTIONS (mainly used by parser)                            */
/* ------------------------------------------------------------------------ */

/* L v1, v2,...vk . E = L v1 . (L v2 ... (L vk . E)).
   `vars' is list of variables in reversed order: vk,...,v2,v1.
   Returns `E' when `vars' is empty list.
   Destroys `vars'.
*/
L_EXPR
L_mk_abs(LIST vars, L_EXPR E)
{
  while (vars) E = ABS1(pop_cont(&vars), E);
  return E;
}

/* Returns application (rator rand). */
L_EXPR L_mk_app(L_EXPR rator, L_EXPR rand) { return APP2(rator, rand); }

/* Returns composition f . g; Note: (f . g) x = f (g x) = B f g x. */
L_EXPR L_mk_comp(L_EXPR f, L_EXPR g) { return COMP(f, g); }

/* Returns consing of car with cdr: (cons car) cdr. */
L_EXPR L_mk_cons(L_EXPR car, L_EXPR cdr) { return CONS(car, cdr); }

/* Expands list comprehension generator abbreviation:
   vn, ..., v2,v1 <- L ==> (v1 <- L), (v2 <- L), ... (vn <- L)
   Destroys `vars'.
*/
LIST
L_mk_vEs (LIST vars, L_EXPR L)
{
  LIST r = NULL_LIST;

  while (vars)
    r = push_cont(mk_vE_cell(pop_cont(&vars), L), r);
  return r;
}

/* Returns (: En  ... (: E2 (: E1 E))...), i.e.,
   when eventually reduced, constructed list will be reverse of arg list.
   Destroys `Ei'.
*/
L_EXPR
L_mk_list(LIST Ei, L_EXPR E)
{
  while (Ei) E = CONS(pop_cont(&Ei), E);
  return E;
}

/* Unravels the list of pairs Ds = (vs Bs) into separate lists vs and Bs.
   Destroys list Ds. Preserves indexing order in the lists.
*/
static void
unzip(LIST Ds, LIST *vs, LIST *Bs)
{
  *vs = *Bs = NULL_LIST;
  while (Ds) {
    L_EXPR Di = pop_cont(&Ds);

    *vs = append_cont((void *) L_vE_v(Di), *vs);
    *Bs = append_cont((void *) L_vE_E(Di), *Bs);
    free_cell(Di);
  }
}

/* unpack n f = L a . f a!0 a!1 ... a!(n-1) where ! means indexing of list.
   (a does not occur free in f).
   n = 0 ==> L a . f
   n = 1 ==> L a . f ((car . I) a) = L a . f (car a)
   n = 2 ==> L a . f (car a) ((car . cdr) a)
*/
static L_EXPR
unpack(int n, L_EXPR f)
{
  const char *as = unique_var();
  L_EXPR cdrs = I_comb, a = mk_name_cell(as);

  while (n--) {
    f = APP2(f, APP2(COMP(L_car, cdrs), a));
    if (n) cdrs = COMP(L_cdr, cdrs);
  }
  return ABS1(as, f);
}

/* Makes `v' undefined, i.e., throws away its global definition if any
   is present.
   Gives warning when trying to undefine a name that has no definition.
*/
static void
undef (const char *v)
{
  L_EXPR def = NULL;
  int idx;

  idx = lookup(GlobalDefsTable, v, strlen(v), (void **) &def, LOOKUP);
  if (idx == NOT_PRESENT || !def)
    warning("No definition for `%s' present", v);
  else
    KEYINFO(GlobalDefsTable, idx) = NULL;
}

void
L_undefs(LIST vars)
{
  while (vars) undef((const char *) pop_cont(&vars));
}

/* Makes preparations for redefinition of `v': checks whether previous
   definition exists, and if so, removes that and issues warning.
   Returns index of `v' in GlobalDefsTable.
*/
static int
prepare_def(const char *v)
{
  L_EXPR def = NULL;
  int idx;
  int flag = (int) INSERT;

  idx = lookup(GlobalDefsTable, v, strlen(v), (void **) &def, &flag);
  if (flag == ALREADY_PRESENT && def) {
    warning("Redefining `%s'", v);
    /* Get rid of any old definition `def': */
    KEYINFO(GlobalDefsTable, idx) = NULL;
  }
  return idx;
}

/* Associates expression `E' with `idx' in GlobalDefsTable.
   Defining expressions are stored in SK-compiled form.
*/
static L_EXPR
mk_def_idx(int idx, L_EXPR E)
{
  return KEYINFO(GlobalDefsTable, idx) = L_compile(E);
}

/* Associates expression `E' with name `v'.
   Defining expressions are stored in SK-compiled form.
   Gives warning when trying to redefine a name that already has a definition.
*/
static L_EXPR
mk_def(const char *v, L_EXPR E)
{
  return mk_def_idx(prepare_def(v), E);
}

/* Global definition: `D' = [[v = B]]. Destroys `D'. */
L_EXPR
L_mk_def(L_EXPR D)
{
  const char *v = L_vE_v(D);
  L_EXPR      B = L_vE_E(D);

  free_cell(D);
  return mk_def(v, B);
}

/* Global definitions: `Ds' = [[{ vi = Bi }]]. Destroys `Ds'.
   No action when `Ds' == NULL_LIST.
*/
void
L_mk_defs(LIST Ds)
{
  while (Ds) L_mk_def(pop_cont(&Ds));
}

/* Global letrec v = B ==> let v = Y (L v.B). */
static L_EXPR
mk_defrec(const char *v, L_EXPR B)
{
  return mk_def(v, Y_APP(ABS1(v, B)));
}

/* Global recursive definition: letrec `D' = [[v = B]] ==> def v = Y (L v.B).
   Will be optimized to def v = B, in case v does not occur free in B.
   Destroys `D'.
*/
L_EXPR
L_mk_defrec(L_EXPR D)
{
  const char *v = L_vE_v(D);
  L_EXPR      B = L_vE_E(D);

  free_cell(D);
  return L_occurs_free(v, B) ? mk_defrec(v, B) : mk_def(v, B);
}

/* Global recursive definitions: letrec [[{ vi = Bi }]].
   Note: vs and Bs are in reversed order: vn=Bn, ..., v2=B2, v1=B1.
   Destroys `vs' and `Bs'.
   Pre: n = |vs| = |Bs| > 1
*/
static void
mk_defrecs(int n, LIST vs, LIST Bs)
{
  /* f = Y (UNPACK-n (L vs . [ Bs ])): */
  L_EXPR f = Y_APP(unpack(n, L_mk_abs(copy_list(vs, 0),
				      L_mk_list(Bs, L_Nil))));
  L_EXPR cdrs = I_comb;

  vs = reverse_list(vs);

  /* Make all mutually dependent defs undefined in one go: */
  FOR_EACH_LIST_ELEM(vs, e)
    ELEM_CONTENTS(e) =
      (void *) prepare_def((const char *) ELEM_CONTENTS(e));
  END_FOR_EACH_LIST_ELEM;

  /* Compile shared sub-expression right away: */
  f = L_compile(f);
  /* Avoid recompilation: */
  L_SET_CFLAG(f);

  while (n--) {
    mk_def_idx((int) pop_cont(&vs), APP2(COMP(L_car, cdrs), f));
    if (n) cdrs = COMP(L_cdr, cdrs);
  }
  L_RESET_CFLAG(f);
}

/* Global recursive definitions: letrec `Ds' = [[{ vi = Bi }]].
   Note: vs and Bs are in reversed order: vn=Bn, ..., v2=B2, v1=B1.
   Destroys `Ds'.
   Pre: n = |Ds| >= 1.
   If n = 1 returns L_mk_defrec result.
   Otherwise does dependency analysis to determine minimum number
   of required defrecs.
*/
void
L_mk_defrecs(LIST Ds)
{
  int n = LIST_SIZE(Ds);
  LIST vs, Bs;

  if (n == 0) return;		/* should never occur */
  if (n == 1) {
    L_mk_defrec(pop_cont(&Ds));
    return;
  }

  unzip(Ds, &vs, &Bs);

  Ds = depend_analysis (n, vs, Bs, 1 /*reverse topo-order*/);
  /* Ds now is list of lists. */
  while (Ds) {
    LIST Di = (LIST) pop_cont(&Ds);
    /* Note: any defs that Di depends on are already treated. */
    
    n = LIST_SIZE(Di);

    /* LIST_INFO indicates self-loop */
    if (n > 1 || LIST_INFO(Di)) {

#if 0
      fputs("letrec\n", stderr);
      FOR_EACH_LIST_ELEM(Di, e) {
	L_EXPR E = (L_EXPR) ELEM_CONTENTS(e);

	fprintf(stderr, "%s = ", L_vE_v(E));
	L_print(stderr, L_vE_E(E));
	putc(LIST_NEXT(e) ? ',' : ';', stderr);
	putc('\n', stderr);
      } END_FOR_EACH_LIST_ELEM;
#endif

      unzip(Di, &vs, &Bs);
      mk_defrecs(n, vs, Bs);
    }
    else /* n == 1 and non-recursive */
      L_mk_defs(Di);
  }
}

/* let v = B in E ==> (L v.E) B */
static L_EXPR
mk_let(const char *v, L_EXPR B, L_EXPR E)
{
  return APP2(ABS1(v, E), B);
}

/* let D in E. D is single definition: v = B. Destroys `D'. */
L_EXPR
L_mk_let(L_EXPR D, L_EXPR E)
{
  const char *v = L_vE_v(D);
  L_EXPR      B = L_vE_E(D);

  free_cell(D);
  return mk_let(v, B, E);
}

/* let v1=B1, v2=B2, ... in E ==> let v1=B1 in let v2=B2 in... in E.
   Note: Ds are in reversed order: vn=Bn, ..., v2=B2, v1=B1.
   if Ds == NULL_LIST returns E. Destroys `Ds'.
*/
L_EXPR
L_mk_lets(LIST Ds, L_EXPR E)
{
  while (Ds) E = L_mk_let(pop_cont(&Ds), E); 
  return E;
}

/* letrec v = B in E ==> let v = Y (L v.B) in E. */
static L_EXPR
mk_letrec(const char *v, L_EXPR B, L_EXPR E)
{
  return mk_let(v, Y_APP(ABS1(v, B)), E);
}

/* letrec v = B in E. Will be optimized to let v = B in E, in case v does not
   occur free in B. Destroys `D'.
*/
L_EXPR
L_mk_letrec(L_EXPR D, L_EXPR E)
{
  const char *v = L_vE_v(D);
  L_EXPR      B = L_vE_E(D);

  free_cell(D);
  return L_occurs_free(v, B) ? mk_letrec(v, B, E) : mk_let(v, B, E);
}

/* Mutually recursive letrec { vi = Bi } in E
   ==> (UNPACK-n (L vs . E)) (Y (UNPACK-n (L vs . [ Bi ]))).
   Destroys `vs' and `Bs'.
   Pre: n = |vs| = |Bs| > 1.
   Note: vs and Bs are in reversed order: vn=Bn, ..., v2=B2, v1=B1.
*/
static L_EXPR
mk_letrecs (int n, LIST vs, LIST Bs, L_EXPR E)
{
  /* f = UNPACK-n (L vs . E) = L w . (L vs . E) w!0 w!1 ...: */
  L_EXPR f = unpack(n, L_mk_abs(copy_list(vs, 0), E));

  /* f (Y (UNPACK-n (L vs . [ Bi ]))): */
  return APP2(f, Y_APP(unpack(n, L_mk_abs(vs, L_mk_list(Bs, L_Nil)))));
}

/* letrec { vi=Bi } in E; i = 1,2,3..n.
   Note: Ds are in reversed order: vn=Bn, ..., v2=B2, v1=B1.
   If Ds = empty list then E is returned;
   if Ds = singleton list [ D1 ] then returns L_mk_letrec (D1, E).
   Otherwise does dependency analysis to determine minimum number
   of required letrecs. Destroys `Ds'.
*/
L_EXPR
L_mk_letrecs(LIST Ds, L_EXPR E)
{
  int n = LIST_SIZE(Ds);
  LIST vs, Bs;

  if (n == 0) return E;		/* should never occur */
  if (n == 1) return L_mk_letrec(pop_cont(&Ds), E);

  unzip(Ds, &vs, &Bs);

  Ds = depend_analysis(n, vs, Bs, 0 /*topo-order*/);
  /* Ds now is list of lists. */
  while (Ds) {
    LIST Di = (LIST) pop_cont(&Ds);
    /* Note: any defs that Di depends on will be treated later.
       (we create the innermost let/letrecs first)
    */

    n = LIST_SIZE(Di);

    /* LIST_INFO indicates self-loop */
    if (n > 1 || LIST_INFO(Di)) {
      unzip(Di, &vs, &Bs);
      E = mk_letrecs(n, vs, Bs, E);
    }
    else /* n == 1 and non-recursive */
      E = L_mk_lets(Di, E);
  }
  return E;
}

/* Dotdot notation for lists.
   [ E2 .. En ]     ==> [ E2, E2+1, ..., En ], if En >= E2
   [ E2 .. ]        ==> [ E2, E2+1, ... ] (infinite progression)
   [ E1, E2 .. En ] ==> [ E1, E2, E2+(E2-E1), ... <=En ]
   [ E1, E2 .. ]    ==> [ E1, E2, E2+(E2-E1), ... ] (infinite progression)

   no E1 then E1 = E2, incr = 1 else incr = E2 - E1

   incr > 0, ascending
   incr < 0, descending
   incr = 0, constant

   if En present:
   letrec G = L n . if (if (> incr 0) <= >=) n En)
                      [ n : G (+ n (- E2 E1)) ] [] in G E1

   if En not present:
   letrec G = L n . [ n : G (+ n (- E2 E1)) ] in G E1
*/
L_EXPR
L_mk_dotdot(L_EXPR E1, L_EXPR E2, L_EXPR En)
{
  /* Should use combinators instead! */
  const char *ns = unique_var();
  const char *Gs = unique_var();
  L_EXPR n = mk_name_cell(ns);
  L_EXPR G = mk_name_cell(Gs);
  L_EXPR incr, c, r;

  /* no E1 then E1 = E2, incr = 1 else incr = E2 - E1 */
  if (!E1) {
    E1 = E2;
    incr = L_1;
  }
  else
    incr = MINUS(E2, E1);

  /* c = [ n : G (+ n incr) ] */
  c = CONS(n, APP2(G, PLUS(n, incr)));
  /* r = G E1 */
  r = APP2(G, E1);

  if (En) {
    /* t = ((if (> incr 0) <= >=) n En) */
    L_EXPR t = APP3(IF(GT(incr, L_0), L_le, L_ge), n, En);
    /* letrec G = L n . if t c [] in r */
    return mk_letrec(Gs, ABS1(ns, IF(t, c, L_Nil)), r);
  }
  /* letrec G = L n . c in r */
  return mk_letrec(Gs, ABS1(ns, c), r);
}

/* TQ[[ [ E | Q ] ++ L2 ]] */
static L_EXPR
mk_list_compr(L_EXPR E, LIST Q, L_EXPR L2)
{
  if (Q) {
    L_EXPR B = (L_EXPR) pop_cont(&Q);

    if (L_vE_P(B)) {		/* B is a generator: v <- L1. */
      /* Should use combinators instead of fresh variables! */
      const char  *v = L_vE_v(B);
      const char *hs = unique_var();
      const char *us = unique_var();
      L_EXPR L1 = L_vE_E(B);
      L_EXPR h  = mk_name_cell(hs);
      L_EXPR u  = mk_name_cell(us);

      /* TQ[[ [ E | v <- L1; Q] ++ L2 ]] ==
	 letrec h = L u . if (null u) L2
                          (let v = car u in TQ[[ [ E | Q ] ++ (h (cdr u)) ]])
         in (h L1);
      */
      return mk_letrec(hs, ABS1(us, IF(NILP(u), L2,
	       mk_let(v, CAR(u), mk_list_compr(E, Q, APP2(h, CDR(u)))))),
		       APP2(h, L1));
    }
    /* B is a filter. */
    /* TQ[[ [ E | B; Q ] ++ L2 ]] == if B TQ[[ [ E | Q ] ++ L2 ]] L2 */
    return IF(B, mk_list_compr(E, Q, L2), L2);
  }
  /* TQ[[ [ E | ] ++ L2 ]] == [ E : L2 ] */
  return CONS(E, L2);
}

/* List comprehension: TE[[ [ E | Q ] ]].
   Reduction rules are:
   [ E | v <- []; Q ]      --> []
   [ E | v <- [E':L']; Q ] --> [ E | Q ][E'/v] ++ [ E | v <- L'; Q ]
   [ E | false; Q ]        --> []
   [ E | true; Q ]         --> [ E | Q ]
   [ E | ]                 --> [ E ]

   Destroys `Q'.
*/
L_EXPR
L_mk_list_compr(L_EXPR E, LIST Q)
{
  /* TE[[ [ E | Q ] ]] == TQ[[ [ E | Q ] ++ [] ]] */
  return mk_list_compr(E, Q, L_Nil);
}

/* Load the FUN definitions in file `filename'.
   Potentially this function and also the parser is called recursively.
   Must therefore use bison instead of regular yacc!
   Using yacc cannot have nested loads.
   If `silent' is non-0 will not complain about errors in opening the file.
   Returns 1 upon successful load, else 0.
*/
Bool
L_load(const char *filename, Bool silent)
{
  FILE *fp;

  if (fun_verbose1) fprintf(stderr, "Loading file %s...\n", filename);

  if (!(fp = fopen(filename, "r"))) {
    if (silent) {
      if (fun_verbose1)
	fprintf(stderr, "No such file or unreadable.\n", filename);
      return 0;
    }
    fprintf(stderr, "Cannot load from file: %s.\n", filename);
    return 0;
  }

  if (!parse_file(filename, fp)) {
    fprintf(stderr, "Syntax errors detected while loading file: %s.\n",
	     filename);
    fclose(fp);
    return 0;
  }
  if (fun_verbose1)
    fprintf(stderr, "Loading file %s...done.\n", filename);
  fclose(fp);
  return 1;
}

static Nat start_of_userdefs = 0;

void
L_save(const char *filename)
{
  FILE *fp;

  if (!(fp = fopen(filename, "w")))
    fprintf(stderr, "Cannot save to file: %s.\n", filename);
  else {
    int nr_items = GlobalDefsTable->nr_items;
    Nat i;
    L_EXPR def = NULL;

    for (i = start_of_userdefs;
	 i < nr_items && !(def = KEYINFO(GlobalDefsTable, i));
	 i++);

    if (def) {
      fputs("letrec\n", fp);
      fputs(KEYSTR(GlobalDefsTable, i), fp);
      fputs(" = ", fp);
      L_print(fp, def);

      do {
	def = NULL;
	for (++i; i < nr_items && !(def = KEYINFO(GlobalDefsTable, i)); i++);

	if (def) {
	  fputs(",\n", fp);
	  fputs(KEYSTR(GlobalDefsTable, i), fp);
	  fputs(" = ", fp);
	  L_print(fp, def);
	}
	else break;
      } forever;
      fputs(";\n", fp);
    }
    fclose(fp);
  }
}

/* Returns FUN list of strings of the standard definitions' names. */
static L_EXPR
mk_stddefs(void)
{
  L_EXPR list = L_Nil;
  Nat i;

  for (i = 0; i < GlobalDefsTable->nr_items; i++)
    list = mk_cons_cell(mk_str(KEYSTR(GlobalDefsTable, i), L_Nil), list);
  start_of_userdefs = i;

/*
  FOR_EACH_OCCUPIED_BUCKET(GlobalDefsTable, i)
    list = mk_cons_cell(mk_str(KEYSTR(GlobalDefsTable, i), L_Nil), list);
  END_FOR_EACH_OCCUPIED_BUCKET;
*/
  return list;
}

/* Returns FUN list of strings of the built-in functions/operators. */
static L_EXPR
mk_builtins(void)
{
  L_EXPR list = L_Nil;
  Nat i;

  for (i = PF_FIRST; i <= PF_LAST_USER; i++)
    list = mk_cons_cell(mk_str(PF_STR(i), L_Nil), list);
  return list;
}

/* Returns FUN list of strings of the FUN program's cmd line arguments.
   Takes strings literally, i.e., doesn't escape any special chars.
*/
static L_EXPR
mk_argv(int argc, char *argv[])
{
  L_EXPR list = L_Nil;
  int i;

  for (i = argc - 1; i >= 0; i--)
    list = mk_cons_cell(mk_str(argv[i], L_Nil), list);
  return list;
}

void
fun_init(int argc, char *argv[])
{
  /* Guard against multiple initialisations: */
  if (FUN_PACKAGE_INITIALIZED) {
    fprintf(stderr, "[fun_init]: Package already initialized.\n");
    return;
  }

  if (fun_verbose0)
    fprintf(stderr,
"[fun_init]: v1.3 Copyright (C) 1995-1997 G. Janssen, Eindhoven University\n");

  cell_verbose = fun_verbose1;
  cell_debug   = reduce_debug = fun_debug;

  cell_init();
  reduce_init();

  L_Builtins = mk_builtins();
  L_Argv     = mk_argv(argc, argv);

  if (!fun_virginal) {
    const char *stddefs = getenv("FUN_STDDEFS");
    const char *home = getenv("HOME");

    L_load(stddefs ? stddefs : FUN_STDDEFS, 1);
    L_Stddefs = mk_stddefs();
    gc();

    if (home) {
      char buf[BUFSIZ];

      strcpy(buf, home);
      strcat(buf, "/.fun");

      L_load(buf, 1);
      gc();
    }
  }
  FUN_PACKAGE_INITIALIZED = 1;
}

void
fun_quit(void)
{
  /* Guard against quit without init (includes multiple quits): */
  if (!FUN_PACKAGE_INITIALIZED)
    fprintf(stderr, "[fun_quit]: Package not initialized.\n");
  else {
    FUN_PACKAGE_INITIALIZED = 0;

    fflush(stdout);

    if (fun_debug) cell_quit();
  }
}

void
L_compile_reduce_print(FILE *fp, L_EXPR E)
{
  clock_t start_t = clock();

  if (!E) return;

  if (fun_verbose1) {
    L_EXPR X;

    X = L_NAM_P(E) ? get_def(L_NAME(E)) : NULL;
    if (X) E = X;

    fputs("Fully parenthesized:\n", fp);
    L_print_intern(fp, E);
    putc('\n', fp);
    fputs("Minimally parenthesized:\n", fp);
    L_print(fp, E);
    putc('\n', fp);
    if (X) {
      fputs("Definition is already in compiled form.\n", fp);
      goto reduce;
    }
  }

  E = L_compile(E);

  if (fun_verbose1) {
    fputs("SK-compiled:\n", fp);
    L_print(fp, E);
    putc('\n', fp);

reduce:
    fputs("Reduced (lazy):\n", fp);
    reduce_verbose = 1;
  }

  gc_on = 1;
  E = L_reduce(isatty(fileno(yyin)) ? fp : NULL, E);
  gc();
  gc_on = 0;

  /*
  if (isatty(fileno(yyin))) {
    L_print(fp, E);
    fputc('\n', fp);
  }
  */

  if (fun_verbose0) {
    fprintf(stderr,
	    "(R:%d; S:%d; GC:%d; C:%da, %du, %dkb; Time:%ds)\n",
	    nr_reductions, stack_ceiling, nr_gc_calls,
	    nr_cells_allocated, nr_cells_in_use,
	    nr_blocks_allocated * SIZEOF_BLOCK / 1024,
	    (clock() - start_t) / CLOCKS_PER_SEC);
  }
  unique_var_count = 0;
}
