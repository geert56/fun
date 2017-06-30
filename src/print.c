/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : print.c
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
#include "print.h"

/* ------------------------------------------------------------------------ */
/* IMPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */

static int output_len;
static int output_limit;

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

static void L_print_1(FILE *fp, L_EXPR E);

/* ------------------------------------------------------------------------ */
/* PRINT FUNCTIONS                                                          */
/* ------------------------------------------------------------------------ */

/* Tests whether list E is entirely made up of characters.
   If so, returns the length of the list, else 0.
   In case 0 is returned this means that either the argument was the
   empty string ("" = []) or a list that doesn't have all chars in it.
   The distinction between them must be done outside this function.
   Pre: L_CONS_P (E).
*/
int
L_stringp(L_EXPR E)
{
  L_EXPR T = E;
  int len = 0;

  L_SET_PFLAG(T);

  while (L_CHAR_P(L_CAR(T))) {
    T = L_CDR(T);

    if (L_PFLAGGED(T)) {
      len = INT_MAX;
      break;
    }

    if (!L_CONS_P(T)) {
      len = 0;
      break;
    }
    len++;
    if (L_NIL_P(T)) break;
  }

  T = E;
  while (L_CONS_P(T) && !L_NIL_P(T) && L_PFLAGGED(T)) {
    L_RESET_PFLAG(T);
    T = L_CDR(T);
  }
  return len;
}

/* Converts any ASCII character `c' to a printable representation as defined
   by the C language. `str_flag' true indicates `c' is to appear in a string,
   else it's assumed to appear as a single character. This ensures that
   the various quote characters are properly dealt with:
   ' = '\'' = "'", " = '"' = "\"".
   Return value is small static char buffer which is '\0' terminated.
*/
char *
string_char(int c, int str_flag)
{
  /* 8 is more than we will ever need. */
  static char buf[8];
  char *p = buf;

  if (isascii(c)) {
    if (c == '\\' || c == '"' && str_flag || c == '\'' && !str_flag)
      /* A backslash must always be quoted; a single-quote is quoted when
	 a char constant, whereas a double-quote must be quoted in a
	 string constant.
      */
      *p++ = '\\';
    else
    if (iscntrl(c)) {
      /* Any control-character will be quoted. */
      *p++ = '\\';
      /* Some have special notation. */
      switch (c) {
      case '\a': c = 'a'; break; /* alarm */
      case '\b': c = 'b'; break; /* backspace */
      case '\f': c = 'f'; break; /* formfeed */
      case '\n': c = 'n'; break; /* newline */
      case '\r': c = 'r'; break; /* return */
      case '\t': c = 't'; break; /* tab */
      case '\v': c = 'v'; break; /* vertical tab */
      /* Others are simply denoted by an octal number. */
      default:
	sprintf(p, "%o", c);
	return buf;
      } /*switch*/
    }
    *p++ = c;
    *p = '\0';
  }
  else
  /* Oops, not an ascii char. */
  if (c < 0 || c > 255)
    /* shows up as \??? */
    sprintf(p, "\\???");
  else
    sprintf(p, "\\%o", c);
  return buf;
}

/* Returns the escaped representation of the character `c' as a FUN list.
   When non-NULL, argument `cdr' is set to point to the cdr of the last
   cons cell.
*/
static L_EXPR
L_show_string_char(int c, L_EXPR **cdr)
{
  char  *p = string_char(c, 1);
  L_EXPR E = mk_str(p, NULL);

  if (cdr) {
    *cdr = &E;
    do
      *cdr = &L_CDR(**cdr);
    while (**cdr);
  }
  return E;
}

L_EXPR
L_show_string(L_EXPR E, L_EXPR cdr)
{
  Bool save_gc_on = gc_on;
  L_EXPR  list;
  L_EXPR *tail = &list;
  L_EXPR cell;

  gc_on = 0;

  cell = mk_cell(L_CONS_T);
  L_CAR(cell) = mk_char_cell('\"'); /* " just to help Emacs C-mode a bit */
  *tail = cell;
  tail = &(L_CDR(cell));

  while (!L_NIL_P(E)) {
    L_EXPR *end;

    *tail = L_show_string_char(L_CHAR(L_CAR(E)), &end);
    tail = end;
    E = L_CDR(E);
    if (!L_CONS_P(E)) break;
  }
  cell = mk_cell(L_CONS_T);
  L_CAR(cell) = mk_char_cell('\"'); /* " just to help Emacs C-mode a bit */
  *tail = cell;
  tail = &(L_CDR(cell));
  *tail = cdr;

  gc_on = save_gc_on;
  return list;
}

/* Outputs string `str' to open file pointer `fp' but only when global
   `output_limit' will not be exceeded.
   Increments `output_len' with length of `str' in all cases.
*/
static void
outstr(FILE *fp, const char *str)
{
  int len = strlen(str);

  if (output_len + len <= output_limit)
    fputs(str, fp);
  output_len += len;
}

/* Prints a FUN list `str' which is assumed to be a non-empty string, to the
   open file pointer `fp'. The characters in `str' will be enclosed in double
   quotes and if necessary escaped.
*/
static void
L_print_string(FILE *fp, L_EXPR str)
{
  outstr(fp, "\"");
  do {
    outstr(fp, string_char(L_CHAR(L_CAR(str)), 1));
    str = L_CDR(str);
  } while (!L_NIL_P(str));
  outstr(fp, "\"");
}

void
L_print_string_raw(FILE *fp, L_EXPR str)
{
  do {
    putc(L_CHAR(L_CAR(str)), fp);
    str = L_CDR(str);
  } while (!L_NIL_P(str));
}

char *
L_sprint_string(char *buf, L_EXPR str, Bool escape)
{
  char *p = buf;

  if (escape)
    do {
      char *cs = string_char(L_CHAR(L_CAR(str)), 1);
      int  len = strlen(cs);

      strcpy(p, cs);
      p += len;
      str = L_CDR(str);
    } while (!L_NIL_P(str));
  else
    do {
      *p++ = L_CHAR(L_CAR(str));
      str = L_CDR(str);
    } while (!L_NIL_P(str));
  *p = '\0';
  return buf;
}

#if 0
static int
L_show_atom_len(L_EXPR E)
{
  switch (L_TYPE(E)) {
  case L_BOOL_T:
    return L_BOOL(E) ? 4 : 5;
  case L_CHAR_T:
    return 1;
  case L_INT_T:
    return 10;
  case L_FLT_T:
    /* DBL_MIN = +2.2250738585072014e-308
       DBL_MAX = +1.7976931348623157e+308
                 123456789012345678901234
    */
    return 24;
  case L_NAM_T:
    return strlen(L_NAME(E));
  default:
    return 0;
  } /*switch*/
  return buf;
}
#endif

char *
L_show_atom(char *buf, L_EXPR E)
{
  switch (L_TYPE(E)) {
  case L_BOOL_T:
    strcpy(buf, L_BOOL(E) ? "True" : "False");
    break;
  case L_CHAR_T:
    strcpy(buf, "\'");
    strcat(buf, string_char(L_CHAR(E), 0));
    strcat(buf, "\'");
    break;
  case L_INT_T:
/*    sprintf(buf, "%d", L_INT(E));*/
    strcpy(buf, l2toa(L_INT(E)));
    break;
  case L_FLT_T:
    strcpy(buf, dtoa(L_FLT(E)));
    break;
  default:
    return NULL;
  } /*switch*/
  return buf;
}

#if 0
static int
L_put_data_raw(FILE *fp, L_EXPR E)
{
  switch (L_TYPE(E)) {
  case L_BOOL_T:
    fputs(L_BOOL(E) ? "True" : "False", fp);
    break;
  case L_CHAR_T:
    putc(L_CHAR(E), fp);
    break;
  case L_INT_T:
/*    fprintf(fp, "%d", L_INT(E));*/
    fputs(l2toa(L_INT(E)), fp);
    break;
  case L_FLT_T:
    fputs(dtoa(L_FLT(E)), fp);
    break;
  case L_CONS_T:
    if (L_NIL_P(E))
      break;
    if (L_stringp(E))
      L_print_string_raw(fp, E);
    /* FALL THROUGH */
  default:
    return 0;
  } /*switch*/
  return 1;
}
#endif

Bool
L_print_atom(FILE *fp, L_EXPR E)
{
  switch (L_TYPE(E)) {
  case L_PF_T:
    outstr(fp, PF_STR(L_PF(E)));
    break;
  case L_NAM_T:
    outstr(fp, L_NAME(E));
    break;
  case L_BOOL_T:
    outstr(fp, L_BOOL(E) ? "True" : "False");
    break;
  case L_CHAR_T:
    outstr(fp, "\'");
    outstr(fp, string_char(L_CHAR(E), 0));
    outstr(fp, "\'");
    break;
  case L_INT_T:
    outstr(fp, l2toa(L_INT(E)));
    break;
  case L_FLT_T:
    outstr(fp, dtoa(L_FLT(E)));
    break;
  case L_CONS_T:
    if (L_NIL_P(E))
      outstr(fp, "[]");
    else
    if (L_stringp(E))
      L_print_string(fp, E);
    else
      return 0;
    break;
  case L_FILE_T:
    outstr(fp, "<file: ");
    outstr(fp, L_FILE_NAM(E));
    outstr(fp, ">");
    break;
  case L_COMB_T:
    if (L_COMB(E)) {
      outstr(fp, COMB_STR(L_COMB(E)));
      return 1;
    }
    /* FALL THROUGH */
  default:
    return 0;
  } /*switch*/
  return 1;
}

/* Prints the list E to open file fp using print_func to print the elements.
   Can handle so-called dotted lists, i.e., non-Nil last cdr.
   Avoids printing circular structures; indicates them by ellipsis "...".
   Pre: L_CONS_P(E)
*/
static void
L_print_list(FILE *fp, L_EXPR E, void (*print_func)(FILE *fp, L_EXPR E))
{
  L_EXPR car = L_CAR(E);
  L_EXPR cdr = L_CDR(E);

  L_SET_PFLAG(E);

  /* Circularity via car not expected; just to be safe! */
  if (L_PFLAGGED(car))
    outstr(fp, "...");
  else
    print_func(fp, car);

  if (L_PFLAGGED(cdr))
    /* Circular list. */
    outstr(fp, ", ...");
  else
  if (L_CONS_P(cdr)) {
    if (!L_NIL_P(cdr)) {
      outstr(fp, ", ");
      L_print_list(fp, cdr, print_func);
    }
  }
  else { /* `dotted list' */
    outstr(fp, " : ");
    print_func(fp, cdr);
  }
  L_RESET_PFLAG(E);
}

static void
L_print_intern_aux(FILE *fp, L_EXPR E)
{
  if (L_print_atom(fp, E)) return;

  switch (L_TYPE(E)) {
  case L_CONS_T:
    outstr(fp, "[ ");
    L_print_list(fp, E, L_print_intern_aux);
    outstr(fp, " ]");
    break;

  case L_APP_T:
    {
      L_EXPR rator = L_RATOR(E);
      L_EXPR  rand = L_RAND (E);

      L_SET_PFLAG(E);

      outstr(fp, "(");

      /* Circularity via rator not expected; just to be safe! */
      if (L_PFLAGGED(rator))
	outstr(fp, "...");
      else
	L_print_intern_aux(fp, rator);

      outstr(fp, " ");

      if (L_PFLAGGED(rand))
	outstr(fp, "(Y (...))");
      else
	L_print_intern_aux(fp, rand);

      outstr(fp, ")");
      L_RESET_PFLAG(E);
    }
    break;

  case L_ABS_T:
    outstr(fp, "(L ");
    outstr(fp, L_BV(E));
    outstr(fp, ".");
    L_print_intern_aux(fp, L_BODY(E));
    outstr(fp, ")");
    break;
  } /*switch*/
}

void
L_print_intern(FILE *fp, L_EXPR E)
{
  output_len = 0;
  output_limit = INT_MAX;
  L_print_intern_aux(fp, E);
}

static void
L_print_aux(FILE *fp, L_EXPR E, int most_right)
{
  if (L_print_atom(fp, E)) return;

  switch (L_TYPE(E)) {
  case L_CONS_T:
    outstr(fp, "[ ");
    L_print_list(fp, E, L_print_1);
    outstr(fp, " ]");
    break;

  case L_APP_T:
    L_SET_PFLAG(E);
    {
      L_EXPR rator = L_RATOR(E);
      L_EXPR  rand = L_RAND (E);
      int need_paren = 0;

      /* Circularity via rator not expected; just to be safe! */
      if (L_PFLAGGED(rator))
	outstr(fp, "...");
      else
	L_print_aux(fp, rator, 1);

      outstr(fp, " ");
      if (L_APP_P(rand)) {
	outstr(fp, "(");
	need_paren = 1;
	if (L_PFLAGGED(rand)) {
	  L_RESET_PFLAG(E);
	  outstr(fp, "Y ...)");
	  break;
	}
	/* Already parens for most-right abstraction. */
	most_right = 0;
      }
      L_print_aux(fp, rand, most_right);
      if (need_paren)
	outstr(fp, ")");
    }
    L_RESET_PFLAG(E);
    break;

  case L_ABS_T:
    /* Is this the most-right abstraction that occurs in the rator
       of a combination? If so, need to parenthesise, because abstractions
       tend to stretch as far to the right as possible and that is not
       intended in this case.
    */
    if (most_right) outstr(fp, "(");
    outstr(fp, "L ");
    do {
      outstr(fp, L_BV(E));
      E = L_BODY(E);
      if (!L_ABS_P(E))
	break;
      outstr(fp, " ");
    } forever;
    outstr(fp, ".");
    L_print_aux(fp, E, 0);
    if (most_right) outstr(fp, ")");
    break;
  } /*switch*/
}

/* Non top-level version of L_print. */
static void
L_print_1(FILE *fp, L_EXPR E)
{
  L_print_aux(fp, E, 0);
}

void
L_print(FILE *fp, L_EXPR E)
{
  output_len = 0;
  output_limit = INT_MAX;
  L_print_1(fp, E);
}

void
L_print_trunc(FILE *fp, L_EXPR E, int limit)
{
  output_len = 0;
  output_limit = limit;
  L_print_1(fp, E);
}
