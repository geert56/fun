/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : print.h
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1997 G.L.J.M. Janssen
 date	   :  8-DEC-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef PRINT_H
#define PRINT_H

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* VARIABLES                                                                */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

extern char *string_char(int c, int str_flag);

/* Test whether list E is entirely made up of characters.
   If so, returns the length of the list, else 0. No way to distinguish
   between an empty string ("" = []) and a cons that doesn't have all
   chars in it. Must do that outside this function.
   Pre: L_CONS_P (E).
*/
extern int L_stringp(L_EXPR E);

/* Prints a FUN list `str' which is assumed to be a non-empty string, to the
   open file pointer `fp'.
   The characters in `str' are output as they are (raw mode).
*/
extern void L_print_string_raw(FILE *fp, L_EXPR str);

/* Prints a FUN string `str' into `buf' without the enclosing quotes.
   Assumes `buf' is large enough to accomodate the string.
   (Use return value of L_stringp for that).
   When `escape' is non-0 will make all chars printable, else uses raw chars.
   Will always add a terminating '\0' character.
   Returns `buf'.
*/
extern char *L_sprint_string(char *buf, L_EXPR str, Bool escape);

/* Prints textual representation of atomic expression `E' in `buf'.
   `E's type may be BOOL, CHAR, INT, or FLOAT.
   The textual representation is the way the atom should be denoted
   in the FUN language, so characters will get surrounding single quotes
   and on top of that, special ones will be escaped. Truth values show as
   `False' and `True'. Numbers are represented in decimal.
*/
extern char *L_show_atom(char *buf, L_EXPR E);

/* Copies the string `E', using `cdr' as cdr value of last new cons cell.
   String `E' will be enclosed in quotes and all chars are escaped.
   So, an internal string is converted to the exterior textual representation
   of that string.
   Watch out for gc! For safety it is switched off.
   Pre: L_CONS_P(E).
*/
extern L_EXPR L_show_string(L_EXPR E, L_EXPR cdr);

extern Bool L_print_atom(FILE *fp, L_EXPR E);

/* Prints any expression fully parenthesized.
   Avoids printing circular structures; indicates them by ellipsis "...".
   Pre: E != NULL.
*/
extern void L_print_intern(FILE *fp, L_EXPR E);

/* Prints any expression minimally parenthesized.
   Avoids printing circular structures; indicates them by ellipsis "...".
*/
extern void L_print(FILE *fp, L_EXPR E);

/* Same as L_print but output is truncated to `limit' number of characters.
   Output is token based, so fewer characters than the `limit' might be
   actually written.
*/
extern void L_print_trunc(FILE *fp, L_EXPR E, int limit);

#endif /* PRINT_H */
