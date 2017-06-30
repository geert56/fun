/*
 DOCUMENTATION INFORMATION			                   module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : compile.h
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1997 G.L.J.M. Janssen
 date	   :  8-DEC-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef COMPILE_H
#define COMPILE_H

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

extern Bool compile_verbose;
extern Bool compile_optimise;

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

/* Top-level SK-compiler.

   Compiles lambda-expression `E' into SK combinator expression.
   Returns either atom, or an application of compiled rator and rand,
   or anything that CA returns:

   C[[ Atom    ]] = Atom
   C[[ (E1 E2) ]] = (C[[ E1 ]] C[[ E2 ]])
   C[[ L v . B ]] = CA v [[ C[[ B ]] ]]

   Where CA compiles according the scheme below:

   CA v [[ v       ]] = I
   CA v [[ x       ]] = K x
   CA v [[ (E1 E2) ]] = Opt [[ S (CA v [[ E1 ]]) (CA v [[ E2 ]]) ]]

   Most notably there will be no variables in the compiled result, only
   constants of the various data types or names of built-in operators.
   "Opt" indicates an optimizer function that rewrites certain expressions
   into a simpler form (for faster reduction).
   The global flag `compile_optimise' controls the optimisation.
   When false the compilation result will solely contain the S, K, I
   combinators; when true more efficient combinators might be introduced
   as well.

   Must watch out for global let and letrec definitions.
   It should be possible to locally rebind those and also we should avoid
   doing recompilations. Any free variables occurring in global definitions
   cannot be bound, and thus remain free forever.

   Assumes all L_NAME_MARK values of name cells to be 0.
   Also, assumes all L_NAME_EXTRA fields of name cells to be NULL.
   Same will hold upon return.
   Also assumes that no circular structures are present in its argument `E'.
   For one, this means that it is not safe to recompile any precompiled
   definition, since the possibility exists that it has already been
   reduced and hence might have been turned into a cyclic graph.
   [This depends on whether the COPY_DEFS option is on or off.]
   Also the compiler does not do anything clever with isomorphic subgraphs,
   except for the case of multiple references to a name that is a definition.
   No provisions are present to guard any L_EXPR cells from garbage collection;
   therefore make sure gc is OFF during compilation!
*/
extern L_EXPR L_compile(L_EXPR E);

#endif /* COMPILE_H */
