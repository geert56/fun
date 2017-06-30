/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : cell.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1998 G.L.J.M. Janssen
 date	   :  6-JAN-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "alloc.h"
#include "hash.h"
#include "cell.h"

/* ------------------------------------------------------------------------ */
/* IMPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

#define MALLOC_CELL() \
	( \
	  free_cell_list ? (temp_cell = free_cell_list, \
		      free_cell_list = L_NEXT(free_cell_list), \
		      nr_cells_in_use++, \
		      nr_cells_since_last_gc++, \
		      temp_cell) \
	            : alloc_cell() \
	)

#define CALLOC_CELL() \
	(temp_cell = MALLOC_CELL(), *temp_cell = NULL_CELL, temp_cell)

#define FREE_CELL(v) \
	do { \
	  L_NEXT(v) = free_cell_list; \
	  free_cell_list = v; \
	  nr_cells_in_use--; \
	} once

#define NR_CELLS_PER_BLOCK \
  (((SIZEOF_BLOCK - sizeof(struct block)) / sizeof(struct L_EXPR)) + 1)

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/* List of blocks of cells: */
typedef struct block {
  struct block *next;
/*  int padding[3];??? could be needed to guarantee long long alignment. */
  struct L_EXPR space[1];
} *BLKPTR;

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */

static    Nat nr_cells_since_last_gc = 0;
static    Nat           gc_threshold = 4 * NR_CELLS_PER_BLOCK;
static BLKPTR             all_blocks = NULL;

/* Variables used in managing list of free cells: */
static              L_EXPR free_cell_list = NULL;
static const struct L_EXPR      NULL_CELL = {0};
static              L_EXPR      temp_cell;

static Bool CELL_PACKAGE_INITIALIZED = 0;

/* Unique cells, not to be garbage-collected: */
static HASHTAB *NamesTable;

/* ------------------------------------------------------------------------ */
/* EXPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* Number of cells allocated at one time: */
Nat nr_cells_allocated     = 0;
Nat nr_cells_in_use        = 0;
Nat nr_blocks_allocated    = 0;
Nat nr_gc_calls            = 0;

/* Unique cells, not to be garbage-collected: */
L_EXPR OperatorsTable[PF_LAST+1];

/* Always protected from garbage-collection: */
HASHTAB *GlobalDefsTable;

/* Unique cells, not to be garbage-collected: */

/* Combinator cells: */
L_EXPR  I_comb;
L_EXPR  K_comb;
L_EXPR  S_comb;
L_EXPR  B_comb;
L_EXPR  C_comb;
L_EXPR Sp_comb;
L_EXPR Bs_comb;
L_EXPR Cp_comb;
L_EXPR  Y_comb;

/* Built-in function cells used internally: */
L_EXPR L_null;
L_EXPR L_len;
L_EXPR L_car;
L_EXPR L_cdr;
L_EXPR L_Cons;
L_EXPR L_append;
L_EXPR L_if;
L_EXPR L_plus;
L_EXPR L_minus;
L_EXPR L_lt;
L_EXPR L_le;
L_EXPR L_ge;
L_EXPR L_gt;

/* Constant value cells: */
L_EXPR L_Nil;
L_EXPR L_False;
L_EXPR L_True;
L_EXPR L_0;
L_EXPR L_1;
L_EXPR L_Stddefs;
L_EXPR L_Builtins;
L_EXPR L_Argv;

/* Name strings of combinators: */
const char *const comb_str[] = {
  NULL,  "I",  "K",  "S",  "B",  "C",  "S'",  "B*",  "C'",  "Y"
};

/* Number of arguments required for each combinator: */
const unsigned char comb_arity[] = {
  /* UNUSED, I, K, S, B, C, S', B*, C', Y  */
          0, 1,	2, 3, 3, 3,  4,  4,  4,	1
};

/* Name strings of built-ins: */
const char *const pf_str[] = {
  "!",
  "^",  "*",  "/",  "%",  "+",  "-",
  "<<", ">>",
  "<",  "<=",  ">",  ">=",  "==",  "!=",
  "&&",  "||",  "->",
  "if",
  "Cons",  "car",  "cdr",  "null", "len", "ith",
  "seq",  "force",
  "++",
  "time",
  "read", "write",
  "shows", "stdout", "stderr", "stdin",
  "error",
  "sin", "cos", "tan", "asin", "acos", "atan",
  "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
  "exp", "log", "log10", "log2",
  "sqrt", "ceil", "abs", "floor",
  "::",
  /* Invisible to user: */
  "putc", "getc", "putc"
};

/* Number of args expected for each built-in: */
const unsigned char pf_arity[] = {
  1,				/* ! */
  2, 2, 2, 2, 2, 2,		/* ^  *  /  %  +  - */
  2, 2,				/* <<  >> */
  2, 2, 2, 2, 2, 2,		/* <  <=  >  >=  =  != */
  2, 2, 2,			/* &&  ||  -> */
  3,				/* if */
  2, 1, 1, 1, 1, 2,		/* Cons  car  cdr  null  len  ith */
  2, 1,				/* seq  force */
  2,				/* ++ */
  1,				/* time */
  1, 2,				/* read write */
  2, 1, 1, 0,			/* shows stdout stderr stdin */
  1,				/* error */
  1, 1, 1, 1, 1, 1,		/* sin cos tan asin acos atan */
  1, 1, 1, 1, 1, 1,		/* sinh cosh tanh asinh acosh atanh */
  1, 1, 1, 1,			/* exp log log10 log2 */
  1, 1, 1, 1,			/* sqrt ceil abs floor */
  1,				/* :: (type) */
  /* Invisible to user: */
  2, 1, 2			/* putc getc putc */
};

Bool cell_verbose = 0;
Bool cell_debug   = 0;

struct gcpro *gcprolist = NULL;

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* IMPORTED FUNCTIONS                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* FUNCTION DEFINITIONS                                                     */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LONG LONG SUPPORT (HP-UX & GCC ONLY!)                                    */
/* ------------------------------------------------------------------------ */

#ifdef LONGLONG_INTS
/* Converts optionally signed digits-string `s' to signed long long number. */
IntT
atol2(const char *s)
{
  Nat neg = 0;
  IntT num;

  if (*s == '-') {
    neg++;
    s++;
  }
  else
  if (*s == '+')
    s++;

  num = *s++ - '0';
  while (*s) {
    num *= 10;
    num += *s++ - '0';
  }
  return neg ? -num : num;
}

/* Converts signed long long number `num' to digits-string.
   Prepends a '-' character when `num' is negative.
   Returns pointer to a locally static buffer!
*/
const char *
l2toa(IntT num)
{
  static char buf[32];
  char *p = buf + sizeof(buf);
  Nat neg = 0;

  if (num < (IntT) 0) {
    /* Special case for LONGLONG_MIN; assuming 2's-compl rep. */
    if (num == ((IntT) 1 << 63))
      return "-9223372036854775808";
    neg++;
    num = -num;
  }
  *--p = '\0';
  do {
    *--p = '0' + (num % (IntT) 10);
    num /= (IntT) 10;
  } while (num);
  if (neg) *--p = '-';
  return p;
}
#endif

const char *
dtoa(double num)
{
  static char buf[32];
  char *p = buf;

  p += sprintf(buf, "%.15g", num);
  if (!strpbrk(buf, "e."))
    /* Make sure integral float shows up as float: */
    strcpy(p, ".0");
  return buf;
}

/* ------------------------------------------------------------------------ */
/* SYMBOL TABLE                                                             */
/* ------------------------------------------------------------------------ */

/* Makes sure that a unique (copy of when `dup' true) string `str' resides
   in the NamesTable. Returns the unique string.
*/
static const char *
Canonise_1(const char *str, Bool dup)
{
  int index;
  int len = strlen(str);
  int do_insert = (int) INSERT;

  index = lookup(NamesTable, (char *) str, len, NULL, &do_insert);
  if (do_insert == INDEED_INSERTED)
    /* Make sure NamesTable holds a unique copy of the string: */
    KEYSTR(NamesTable, index) = (char *) (dup ? Strdup(str) : str);
  return KEYSTR(NamesTable, index);
}

const char *Canonise (const char *str) { return Canonise_1(str, 1); }

const char *CanoniseC(const char *str) { return Canonise_1(str, 0); }

/* ------------------------------------------------------------------------ */
/* CELL ALLOCATION & GARBAGE COLLECTION                                     */
/* ------------------------------------------------------------------------ */

/* Pre: E != NULL */
static void
gc_mark_cell(L_EXPR E)
{
 restart:
  if (!L_GCFLAGGED(E)) {
    L_SET_GCFLAG(E);

    switch (L_TYPE(E)) {

    case L_APP_T:
    case L_CONS_T:
      gc_mark_cell(L_RATOR(E));
      E = L_RAND(E);
      goto restart;

    case L_ABS_T: /* == L_COMB_T */
    case L_vE_T:
      if (!L_COMB(E)) {
	E = L_BODY(E);
	goto restart;
      }
      break;

    default:
      break;
    } /*switch*/
  }
}

/* 32 of them is enough for now. */
static L_EXPR *static_gcproed[32] = {NULL};
static L_EXPR **static_gcpro_top = static_gcproed;
#define gcpro_static(E)		(*static_gcpro_top++ = &(E))

static void
gc_mark(void)
{
  if (cell_verbose) fprintf(stderr, " marking...");

  /* Mark the free list cells: (Has to be done first!) */
  /* Why not keep these marked at all times, and only remove mark when
     a cell is removed from it. But then sweeping should not unmark
     cells on the free list and that's hard to detect.
     Anyway, upon gc the free list is usually empty.
  */
  {
    L_EXPR p;

    for (p = free_cell_list; p; p = L_NEXT(p)) L_SET_GCFLAG(p);
  }

  /* Mark all L_EXPR cells associated with built-in operators: */
  /* These definitely could be marked once and for all, but then sweeping
     should not again unmark these cells.
     Could allocate them in separate area, then they are invisible to
     sweeping. Why bother: there are too few cells to merit this.
  */
  {
    int i;

    for (i = PF_FIRST; i <= PF_LAST; i++) gc_mark_cell(OperatorsTable[i]);
  }

  /* Mark all `static' L_EXPR cells for built-in constants etc.: */
  /* These definitely could be marked once and for all, but then sweeping
     should not again unmark these cells.
     Could allocate them in separate area, then they are invisible to
     sweeping. Why bother: there are too few cells to merit this.
  */
  {
    L_EXPR **p;

    for (p = static_gcproed; p < static_gcpro_top; p++) gc_mark_cell(**p);
  }

  /* Mark all name cells: */
  FOR_EACH_OCCUPIED_BUCKET(NamesTable, i) {
    L_EXPR v = KEYINFO(NamesTable, i);

    if (v) L_SET_GCFLAG(v);
  } END_FOR_EACH_OCCUPIED_BUCKET;

  /* Mark all global definitions: */
  FOR_EACH_OCCUPIED_BUCKET(GlobalDefsTable, i) {
    L_EXPR def = KEYINFO(GlobalDefsTable, i);

    if (def) gc_mark_cell(def);
  } END_FOR_EACH_OCCUPIED_BUCKET;

  /* Note: no need to mark the spine stack contents. */

  /* Mark the cells on the gcpro list(dynamically protected cells): */
  {
    struct gcpro *p;

    for (p = gcprolist; p; p = p->next) gc_mark_cell(*(p->var));
  }
}

static void
gc_sweep(void)
{
  BLKPTR block;

  if (cell_verbose) fprintf(stderr, ", sweeping...");

  for (block = all_blocks; block; block = block->next) {
    L_EXPR p = block->space;
    int i;

    for (i = 0; i < NR_CELLS_PER_BLOCK; i++, p++)
      if (L_GCFLAGGED(p))
	L_RESET_GCFLAG(p);
      else {
	/* Special action for FILE cells: close file if not already done: */
	if (L_FILE_P(p) && L_FILE_FP(p))
	  fclose(L_FILE_FP(p));
	FREE_CELL(p);
      }
  }
}

Bool gc_on = 0;
Bool gc_in_progress = 0;

void
gc(void)
{
  gc_in_progress = 1;
  nr_gc_calls++;
  if (cell_verbose) fprintf(stderr, "Garbage collecting:");
  gc_mark();
  gc_sweep();
  if (cell_verbose) fprintf(stderr, "done.\n");
  gc_in_progress = 0;
}

/* Allocate another batch (= block) of cells. */
/* pre: free_cell_list == NULL */
static L_EXPR
alloc_cell(void)
{
  L_EXPR p;
  int i;
  BLKPTR new_blk;

  if (gc_on) {
    if (nr_cells_since_last_gc > gc_threshold) {
      gc();
      nr_cells_since_last_gc = 0;
    }
    if (free_cell_list) return MALLOC_CELL();
  }

  /* Get a fresh block of nodes: */
/*  check_mem_limit(SIZEOF_BLOCK);*/
  new_blk = MA_MALLOC_BYTES(SIZEOF_BLOCK, BLKPTR);
  new_blk->next = all_blocks;
  all_blocks = new_blk;

  nr_blocks_allocated++;

  nr_cells_allocated += NR_CELLS_PER_BLOCK;

  /* Link all new nodes in a list: */
  free_cell_list = p = new_blk->space;
  for (i = 0; i < NR_CELLS_PER_BLOCK-1; i++)
    p = (L_NEXT(p) = p + 1);
  L_NEXT(p) = NULL;

  /* Strip off the first one: */
  p = free_cell_list;
  free_cell_list = L_NEXT(free_cell_list);
  nr_cells_in_use++;
  nr_cells_since_last_gc++;

  return p;
}

void
free_cell(L_EXPR E)
{
  FREE_CELL(E);
}

#if 0
static void
cell_free_blocks(void)
{
  while (all_blocks) {
    BLKPTR save = all_blocks;

    all_blocks = all_blocks->next;
    MA_FREE_BYTES(save, SIZEOF_BLOCK);
  }
  free_cell_list = NULL;
  nr_cells_allocated = 0;
  nr_blocks_allocated = 0;
}
#endif

/* ------------------------------------------------------------------------ */
/* CELL CONSTRUCTOR PRIMITIVES                                              */
/* ------------------------------------------------------------------------ */

/* For descriptive comments on these functions see cell.h. */

L_EXPR
get_def(const char *name)
{
  L_EXPR def = NULL;

  lookup(GlobalDefsTable, name, strlen(name), (void **) &def, LOOKUP);
  return def;
}

static void
set_def(const char *v, L_EXPR E)
{
  lookup(GlobalDefsTable, v, strlen(v), (void **) &E, INSERT);
}

L_EXPR
get_name(const char *name)
{
  L_EXPR cell = NULL;

  lookup(NamesTable, name, strlen(name), (void **) &cell, LOOKUP);
  return cell;
}

L_EXPR
mk_cell(int type)
{
  L_EXPR cell = CALLOC_CELL();

  L_TYPE(cell) = type;
  return cell;
}

L_EXPR
mk_app_cell(L_EXPR E1, L_EXPR E2)
{
  L_EXPR A = CALLOC_CELL();

  L_TYPE (A) = L_APP_T;
  L_RATOR(A) = E1;
  L_RAND (A) = E2;
  return A;
}

/* Returns new empty cell of specified combinator type: */
static L_EXPR
mk_comb_cell(int comb)
{
  L_EXPR cell = CALLOC_CELL();

  L_COMB(cell) = comb;
  return cell;
}

L_EXPR
mk_cons_cell(L_EXPR car, L_EXPR cdr)
{
  L_EXPR C = CALLOC_CELL();

  L_TYPE(C) = L_CONS_T;
  L_CAR (C) = car;
  L_CDR (C) = cdr;
  return C;
}

L_EXPR
mk_file_cell(FILE *fp, const char *fnam)
{
  L_EXPR file = mk_cell(L_FILE_T);

  L_FILE_FP (file) = fp;
  L_FILE_NAM(file) = fnam;
  return file;
}

L_EXPR
mk_abs_cell(const char *v, L_EXPR B)
{
  L_EXPR A = mk_cell(L_ABS_T);

  L_BV  (A) = v;
  L_BODY(A) = B;
  return A;
}

L_EXPR
mk_vE_cell(const char *v, L_EXPR E)
{
  L_EXPR G = mk_cell(L_vE_T);

  L_vE_v(G) = v;
  L_vE_E(G) = E;
  return G;
}

L_EXPR
mk_name_cell(const char *name)
{
  L_EXPR cell = NULL;
  int idx;

  idx = lookup(NamesTable, name, strlen(name), (void **) &cell, LOOKUP);
  if (!cell) {
    cell         = mk_cell(L_NAM_T);
    L_NAME(cell) = name;
    KEYINFO(NamesTable, idx) = cell;
  }
  return cell;
}

L_EXPR
mk_bool_cell(int bool)
{
  L_EXPR cell = mk_cell(L_BOOL_T);

  L_BOOL(cell) = bool;
  return cell;
}

L_EXPR
mk_char_cell(char c)
{
  L_EXPR cell = mk_cell(L_CHAR_T);

  L_CHAR(cell) = (unsigned char) c;
  return cell;
}

L_EXPR
mk_num_cell(IntT num)
{
  L_EXPR cell = mk_cell(L_INT_T);

  L_INT(cell) = num;
  return cell;
}

L_EXPR
mk_float_cell(double num)
{
  L_EXPR cell = mk_cell(L_FLT_T);

  L_FLT(cell) = num;
  return cell;
}

L_EXPR
cp_list(L_EXPR E, L_EXPR cdr)
{
  Bool save_gc_on = gc_on;
  L_EXPR  list;
  L_EXPR *tail = &list;

  gc_on = 0;

  while (!L_NIL_P(E)) {
    L_EXPR cell = mk_cell(L_CONS_T);

    L_CAR(cell) = L_CAR(E);
    *tail = cell;
    tail = &(L_CDR(cell));
    E = L_CDR(E);
    if (!L_CONS_P(E)) break;
  }
  *tail = cdr ? cdr : E;
  gc_on = save_gc_on;
  return list;
}

L_EXPR
mk_str(const char *s, L_EXPR last_cdr)
{
  Bool save_gc_on = gc_on;
  L_EXPR  list;
  L_EXPR *tail = &list;

  gc_on = 0;

  while (*s) {
    L_EXPR cell = mk_cell(L_CONS_T);

    L_CAR(cell) = mk_char_cell(*s++);
    *tail = cell;
    tail = &(L_CDR(cell));
  }
  *tail = last_cdr;
  gc_on = save_gc_on;
  return list;
}

L_EXPR mk_op_cell(int op) { return OperatorsTable[op]; }

static L_EXPR
cp(L_EXPR E)
{
  switch (L_TYPE(E)) {

  case L_APP_T:
    return mk_app_cell(cp(L_RATOR(E)), cp(L_RAND(E)));

  case L_CONS_T:
    return L_NIL_P(E) ? L_Nil : mk_cons_cell(cp(L_CAR(E)), cp(L_CDR(E)));

  case L_ABS_T: /* == L_COMB_T, no abstractions after compilation */
    return L_COMB(E) ? E : mk_abs_cell(L_BV(E), cp(L_BODY(E)));

  case L_vE_T:
    return mk_vE_cell(L_vE_v(E), cp(L_vE_E(E)));

  /* Shared cells! */
  case L_PF_T:
  case L_NAM_T:
  case L_BOOL_T:
  case L_CHAR_T:
  case L_INT_T:
  case L_FLT_T:
  case L_FILE_T:		/* Only lives during reduction */
    return E;
  } /*switch*/
  /* UNREACHABLE */
}

L_EXPR
L_cp(L_EXPR E)
{
  Bool save_gc_on = gc_on;

  gc_on = 0;
  E = cp(E);
  gc_on = save_gc_on;
  return E;
}

void
cell_print_stats(FILE *fp)
{
  fprintf(fp, "(GC:%d; C:%da, %du; %dkb)\n",
	  nr_gc_calls,
	  nr_cells_allocated, nr_cells_in_use,
	  nr_blocks_allocated * SIZEOF_BLOCK / 1024);
}

void
cell_init(void)
{
  int i;

  /* Guard against multiple initialisations: */
  if (CELL_PACKAGE_INITIALIZED) {
    fprintf(stderr, "[cell_init]: Package already initialized.\n");
    return;
  }

  if (cell_verbose)
    fprintf(stderr, "sizeof(struct L_EXPR) = %d bytes.\n",
	    sizeof(struct L_EXPR));

  /* Don't want hash table routines to copy the key string!
     All strings uniquely reside in the NamesTable.
  */
  hash_copy_key   = 0;
  NamesTable      = hashtab_create(0);
  GlobalDefsTable = hashtab_create(0);

  gcpro_static(I_comb);
  gcpro_static(K_comb);
  gcpro_static(S_comb);
  gcpro_static(B_comb);
  gcpro_static(C_comb);
  gcpro_static(Sp_comb);
  gcpro_static(Bs_comb);
  gcpro_static(Cp_comb);
  gcpro_static(Y_comb);

  gcpro_static(L_0);
  gcpro_static(L_1);
  gcpro_static(L_Nil);
  gcpro_static(L_False);
  gcpro_static(L_True);
  gcpro_static(L_Stddefs);
  gcpro_static(L_Builtins);
  gcpro_static(L_Argv);

  for (i = I_COMB; i <= Y_COMB; i++)
    CanoniseC(COMB_STR(i));

  I_comb  = mk_comb_cell(I_COMB);
  K_comb  = mk_comb_cell(K_COMB);
  S_comb  = mk_comb_cell(S_COMB);
  B_comb  = mk_comb_cell(B_COMB);
  C_comb  = mk_comb_cell(C_COMB);
  Sp_comb = mk_comb_cell(Sp_COMB);
  Bs_comb = mk_comb_cell(Bs_COMB);
  Cp_comb = mk_comb_cell(Cp_COMB);
  Y_comb  = mk_comb_cell(Y_COMB);

  for (i = PF_FIRST; i <= PF_LAST_USER; i++) {
    L_EXPR pf = mk_cell(L_PF_T);

    L_PF(pf) = i;
    OperatorsTable[i] = pf;
    /* Predefine all operators: */
    set_def(CanoniseC(PF_STR(i)), pf);
  }
  for (; i <= PF_LAST; i++) {
    L_EXPR pf = mk_cell(L_PF_T);

    L_PF(pf) = i;
    OperatorsTable[i] = pf;
    CanoniseC(PF_STR(i));
  }

  L_null   = OperatorsTable[PF_NULL];
  L_len    = OperatorsTable[PF_LEN];
  L_car    = OperatorsTable[PF_CAR];
  L_cdr    = OperatorsTable[PF_CDR];
  L_Cons   = OperatorsTable[PF_CONS];
  L_append = OperatorsTable[PF_APPEND];
  L_if     = OperatorsTable[PF_IF];
  L_plus   = OperatorsTable[PF_PLUS];
  L_minus  = OperatorsTable[PF_MINUS];
  L_lt     = OperatorsTable[PF_LT];
  L_le     = OperatorsTable[PF_LE];
  L_ge     = OperatorsTable[PF_GE];
  L_gt     = OperatorsTable[PF_GT];

  /* Note: cannot use mk_cons_cell(L_Nil, L_Nil) */
  L_Nil   = mk_cell(L_CONS_T);
  L_CAR(L_Nil) = L_Nil;
  L_CDR(L_Nil) = L_Nil;
  L_NIL_P(L_Nil) = 1;
  L_False = mk_bool_cell(0);
  L_True  = mk_bool_cell(1);
  L_0     = mk_num_cell(0);
  L_1     = mk_num_cell(1);

  L_Builtins = L_Nil;
  L_Stddefs  = L_Nil;
  L_Argv     = L_Nil;

  CELL_PACKAGE_INITIALIZED = 1;
}

void
cell_quit(void)
{
  /* Guard against quit without init(includes multiple quits): */
  if (!CELL_PACKAGE_INITIALIZED)
    fprintf(stderr, "[cell_quit]: Package not initialized.\n");
  else {
    CELL_PACKAGE_INITIALIZED = 0;

    if (cell_debug) {

      {
	int i;

	for (i = PF_FIRST; i <= PF_LAST; i++) OperatorsTable[i] = L_Nil;
      }

      {
	L_EXPR **p;

	for (p = static_gcproed; p < static_gcpro_top; p++)
	  **p = L_Nil;
      }

      FOR_EACH_OCCUPIED_BUCKET(NamesTable, i)
	KEYINFO(NamesTable, i) = NULL;
      END_FOR_EACH_OCCUPIED_BUCKET;

      FOR_EACH_OCCUPIED_BUCKET(GlobalDefsTable, i)
	KEYINFO(GlobalDefsTable, i) = NULL;
      END_FOR_EACH_OCCUPIED_BUCKET;

      assert(gcprolist == NULL);

      gc();
      /* Now there should be only 1 cell still in use: L_Nil. */

      cell_print_stats(stderr);
    }
  }
}
