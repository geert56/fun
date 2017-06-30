/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : scc.h
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1997 G.L.J.M. Janssen
 date	   :  8-DEC-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef SCC_H
#define SCC_H

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

/* Analyses dependencies among the `vs' variables:
   Constructs dependency graph according free occurrences of `vs' in `Bs'.
   An edge vj --> vi in the graph means that vj depends on vi, i.e., vi
   occurs free in the definition Bj of vj.
   Determines strongly connected components and identifies each non-trivial
   one (= size > 1) with one letrec. Singleton components may become simple
   lets, unless they have a self-loop.
   The argument lists `vs' and `Bs' are destroyed.

   Returns a list of lists. Each sub-list has 1 or more vE cells as elements.
   The LIST_INFO field of the sub-list when non-0 indicates that the
   corresponding vertex had a self-loop hence the definition vk = Bk is
   recursive and must be handled by a (let|def)rec.
   A sub-list of more than 1 element specifies a mutually recursive definition
   for sure. All other singleton sub-lists may be treated by lets.
   `rev_result' controls the order of the sub-lists: when true the sub-lists
   are in reverse topological order, otherwise the order is topological.
*/
extern LIST depend_analysis(Nat n, LIST vs, LIST Bs, Bool rev_result);

#endif /* SCC_H */
