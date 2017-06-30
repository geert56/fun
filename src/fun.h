/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : fun.h
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1997 G.L.J.M. Janssen
 date	   :  8-DEC-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef FUN_H
#define FUN_H

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include "cell.h"

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* VARIABLES                                                                */
/* ------------------------------------------------------------------------ */

extern Bool fun_verbose0;
extern Bool fun_verbose1;
extern Bool fun_debug;
extern Bool fun_echo;
extern Bool fun_virginal;

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

extern void fun_init(int argc, char *argv[]);
extern void fun_quit(void);

extern L_EXPR L_mk_def		(L_EXPR D);
extern   void L_mk_defs		(LIST Ds);
extern L_EXPR L_mk_defrec	(L_EXPR D);
extern   void L_mk_defrecs	(LIST Ds);
extern   void L_undefs		(LIST vars);
extern L_EXPR L_mk_let		(L_EXPR D, L_EXPR E);
extern L_EXPR L_mk_lets		(LIST Ds, L_EXPR E);
extern L_EXPR L_mk_letrec	(L_EXPR D, L_EXPR E);
extern L_EXPR L_mk_letrecs	(LIST Ds, L_EXPR E);
extern L_EXPR L_mk_app		(L_EXPR rator, L_EXPR rand);
extern L_EXPR L_mk_comp		(L_EXPR f, L_EXPR g);
extern L_EXPR L_mk_abs		(LIST vars, L_EXPR E);
extern L_EXPR L_mk_cons		(L_EXPR car, L_EXPR cdr);
extern L_EXPR L_mk_list		(LIST Ei, L_EXPR E);
extern L_EXPR L_mk_dotdot	(L_EXPR E1, L_EXPR E2, L_EXPR En);
extern L_EXPR L_mk_list_compr	(L_EXPR E, LIST Q);
extern   LIST L_mk_vEs		(LIST vars, L_EXPR L);

extern Bool L_load         (const char *filename, Bool silent);
extern void L_save         (const char *filename);

extern void L_compile_reduce_print(FILE *fp, L_EXPR E);

#endif /* FUN_H */
